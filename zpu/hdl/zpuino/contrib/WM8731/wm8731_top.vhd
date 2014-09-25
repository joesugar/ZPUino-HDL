---------------------------------------------------------------------
----                                                             ----
----  WISHBONE revB2 compl. WM8731 code core; top level          ----
----                                                             ----
----                                                             ----
----  Author: J. Consugar                                        ----
----                                                             ----
---------------------------------------------------------------------
----                                                             ----
---- Copyright (C) 2014 J. Consugar                              ----
----                                                             ----
---- This source file may be used and distributed without        ----
---- restriction provided that this copyright statement is not   ----
---- removed from the file and that any derivative work contains ----
---- the original copyright notice and the associated disclaimer.----
----                                                             ----
----     THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY     ----
---- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED   ----
---- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS   ----
---- FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL THE AUTHOR      ----
---- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,         ----
---- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    ----
---- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE   ----
---- GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR        ----
---- BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  ----
---- LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY, OR TORT  ----
---- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT  ----
---- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         ----
---- POSSIBILITY OF SUCH DAMAGE.                                 ----
----                                                             ----
---------------------------------------------------------------------
--
-- Change History:
--               Revision 0.1 - J. Consugar
--               Initial coding
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.zpu_config.all;
use work.zpupkg.all;
use work.zpuinopkg.all;

entity wm8731_top is
  generic (
    ARST_LVL : std_logic := '0'                         -- asynchronous reset level
  );
  port (
    -- wishbone signals
    wb_clk_i      : in  std_logic;                      -- master clock input
    wb_rst_i      : in  std_logic := '0';               -- synchronous active high reset
    arst_i        : in  std_logic := not ARST_LVL;      -- asynchronous reset
    wb_adr_i      : in  std_logic_vector(maxIObit downto minIObit);  -- read/write address
    wb_dat_i      : in  std_logic_vector(wordSize-1 downto 0);       -- data in signal
    wb_dat_o      : out std_logic_vector(wordSize-1 downto 0);       -- data out signal
    wb_we_i       : in  std_logic;                      -- Write enable input
    wb_stb_i      : in  std_logic;                      -- Strobe signals / core select signal
    wb_cyc_i      : in  std_logic;                      -- Valid bus cycle input
    wb_ack_o      : out std_logic;                      -- Bus cycle acknowledge output
    wb_inta_o     : out std_logic;                      -- interrupt request output signal

    -- wm8731 lines
    wm_clk_i      : in  std_logic;                      -- codec clock signal
    wm_rst_i      : in  std_logic                       -- codec reset signal
    );
end entity wm8731_top;
    
architecture structural of wm8731_top is
  -- COMPONENTS
  
  -- 
  -- Asynchronous FIFO used to cross domains between the ZPUino and WM8731 codec.
  --
  component async_fifo is
  generic (
    DATA_WIDTH :integer := 16;
    ADDR_WIDTH :integer := 3
  );
  port (
    -- Reading port.
    Data_out    :out std_logic_vector (DATA_WIDTH-1 downto 0);
    Empty_out   :out std_logic;
    ReadEn_in   :in  std_logic;
    RClk        :in  std_logic;
    
    -- Writing port.
    Data_in     :in  std_logic_vector (DATA_WIDTH-1 downto 0);
    Full_out    :out std_logic;
    WriteEn_in  :in  std_logic;
    WClk        :in  std_logic;

    Clear_in    :in  std_logic    -- active hi
  );
  end component;
  
  -- END COMPONENTS
  
  -- SIGNALS
  
  signal codec_clock_counter : unsigned(7 downto 0);      -- counter to create codec clock
  signal codec_clock : std_logic;                         -- codec clock
  
  signal iack_o : std_logic;                              -- internal acknowledgement signal.
  
  signal fifo_full : std_logic;                           -- fifo full flag
  signal fifo_empty : std_logic;                          -- fifo empty flag
  signal fifo_write_data : std_logic_vector(15 downto 0); -- fifo write data
  signal fifo_write_enable : std_logic;                   -- fifo write enable
  signal fifo_write_clear : std_logic;                    -- fifo write address clear
  signal fifo_read_data : std_logic_vector(15 downto 0);  -- fifo read data
  signal fifo_read_enable : std_logic;                    -- fifo read enable
  signal fifo_read_clear : std_logic;                     -- fifo read address clear
  signal fifo_clear : std_logic;                          -- fifo clear signal
  signal fifo_read_flag : std_logic;                      -- fifo read flag
  signal read_address_inc : std_logic;                    -- read address gets incremented when set
  
  -- END SIGNALS
    
begin
  -- PORT MAPPING
  
  -- 
  -- Hookup byte controller block
  --
  fifo : async_fifo
  port map (
    -- Reading port.
    Data_out    => fifo_read_data,
    Empty_out   => fifo_empty,
    ReadEn_in   => fifo_read_enable,
    RClk        => wm_clk_i,
    
    -- Writing port.
    Data_in     => fifo_write_data,
    Full_out    => fifo_full,
    WriteEn_in  => fifo_write_enable, 
    WClk        => wb_clk_i,

    Clear_in    => fifo_clear
  );

  -- END PORT MAPPING
    
  -- PROCESSING
  
  -- 
  -- Tie interrupt to '0', we never interrupt 
  --
  wb_inta_o <= '0';
  
  --
  -- Acknowledge all transfers per the wishbone spec.
  --
  iack_o <= wb_stb_i and wb_cyc_i; 
  wb_ack_o <= iack_o;

  -- 
  -- Initial clear signal for the fifo
  --
  process(wb_clk_i, wb_rst_i)
  begin
    if (wb_rst_i = '1') then
      fifo_write_clear <= '1';
    elsif rising_edge(wb_clk_i) then
      fifo_write_clear <= '0';
    end if;
  end process;

  process(wm_clk_i, wb_rst_i)
  begin
    if (wb_rst_i = '1') then
      fifo_read_clear <= '1';
    elsif rising_edge(wm_clk_i) then
      fifo_read_clear <= '0';
    end if;
  end process;
  
  fifo_clear <= fifo_write_clear or fifo_read_clear;
  
  -- 
  -- Write data to the async fifo.
  --
  write_data: process(wb_rst_i, wb_clk_i)
  begin
    -- 
    -- Reset
    --
    if (wb_rst_i = '1') then
      fifo_write_enable <= '0';
      fifo_write_data <= (others => '0');
    
    -- 
    -- Prepare data for writing.
    --
    elsif rising_edge(wb_clk_i) then
      --
      -- Initialize
      --
      fifo_write_data <= (others => '0');
      fifo_write_enable <= '0';
      
      -- 
      -- Process the different addresses on write
      --
      if (wb_cyc_i='1' and wb_stb_i='1' and wb_we_i='1') then
        case wb_adr_i(minIObit+2 downto minIObit) is
          -- 
          -- write data register.
          --
          when "000" => 
            if (fifo_full = '0') then
              fifo_write_data <= wb_dat_i(15 downto 0);
              fifo_write_enable <= '1';
            end if;
          
          --
          -- Status register.  On write no action.
          --
          when "001" => null;
          
          -- 
          -- Illegal cases, for simulation only
          --
          when others =>
            report ("Illegal write address, setting all registers to unknown.");
            fifo_write_data <= (others => 'X');
            
        end case;
      end if;
    end if;
  end process;
  
  -- 
  -- Load the output data when address is read.
  --
  fifo_read_flag <= iack_o and not(wb_we_i);
  read_data: process(fifo_read_flag)
  begin
    if (fifo_read_flag = '0') then
      read_address_inc <= '0';
    else
      --
      -- Process the different addresses.
      --
      case wb_adr_i(minIObit+2 downto minIObit) is
        --
        -- Read data from the async fifo
        --
        when "000" =>
          wb_dat_o(31 downto 0) <= (others => '0');
          wb_dat_o(15 downto 0) <= fifo_read_data;
          read_address_inc <= '1';
          
        -- 
        -- Read status flags.
        -- 
        when "001" =>
          wb_dat_o(31 downto 0) <= (others => '0');
          wb_dat_o(0) <= fifo_empty;
          wb_dat_o(1) <= fifo_full;
          read_address_inc <= '0';
          
        --
        -- Illegal cases.
        --
        when others =>
          report ("Illegal read address, setting all values to unknown.");
          wb_dat_o(31 downto 0) <= (others => 'X');
          read_address_inc <= '0';
          
      end case;
    end if;
  end process;
  
  --
  -- Process to update the read address.
  --
  process(wb_rst_i, wb_clk_i)
  begin
    if (wb_rst_i = '1') then
      fifo_read_enable <= '0';
    elsif rising_edge(wb_clk_i) then
      fifo_read_enable <= '0';
      if ((fifo_read_flag = '1') and (read_address_inc = '1')) then
        fifo_read_enable <= '1';
      end if;
    end if;
  end process;
  
  -- -- 
  -- -- Create the clock used to drive the codec.
  -- --
  -- process(wm_clk_i, wb_rst_i)
  -- begin
    -- if (wb_rst_i = '0') then
      -- codec_clock_counter <= (others => '0');
    -- elsif rising_edge(wm_clk_i) then
      -- codec_clock_counter <= codec_clock_counter + 1;
    -- end if;
  -- end  process;
  -- codec_clock <= codec_clock_counter(7);
    
  -- END PROCESSING

end architecture structural;

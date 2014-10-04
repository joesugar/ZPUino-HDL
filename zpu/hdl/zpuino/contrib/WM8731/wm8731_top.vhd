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
use ieee.std_logic_misc.all;

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
    wm_rst_i      : in  std_logic;                      -- codec reset signal
    wm_bclk_o     : out std_logic;                      -- codec bclk signal
    wm_lrc_o      : out std_logic;                      -- codec left/right channel
    wm_dacdat_o   : out std_logic                       -- coded DAC data
    );
end entity wm8731_top;
    
architecture structural of wm8731_top is
  -- COMPONENTS
  
  -- 
  -- Asynchronous FIFO used to cross domains between the ZPUino and WM8731 codec.
  --
  component async_fifo is
  generic (
    DATA_WIDTH :integer := wordSize;
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
  
  signal codec_sample_clock_counter : unsigned(7 downto 0);   -- 8-bit counter to create codec clock
  signal codec_sample_clock : std_logic;                      -- codec clock
  signal codec_bclk : std_logic;                              -- codec bclk signal
  
  signal iack_o : std_logic;                                  -- internal acknowledgement signal.
  
  signal d2a_fifo_full : std_logic;                           -- fifo full flag
  signal d2a_fifo_empty : std_logic;                          -- fifo empty flag
  signal d2a_fifo_data_in : std_logic_vector(wordSize-1 downto 0);    -- fifo write data
  signal d2a_fifo_write_enable : std_logic;                   -- fifo write enable
  signal d2a_fifo_write_clear : std_logic;                    -- fifo write address clear
  signal d2a_fifo_data_out : std_logic_vector(wordSize-1 downto 0);   -- fifo data out
  signal d2a_fifo_read_enable : std_logic;                    -- fifo read enable
  signal d2a_fifo_read_clear : std_logic;                     -- fifo read address clear
  signal d2a_fifo_clear : std_logic;                          -- fifo clear signal
  signal d2a_fifo_read_flag : std_logic;                      -- fifo read flag

  signal a2d_fifo_full : std_logic;                           -- fifo full flag
  signal a2d_fifo_empty : std_logic;                          -- fifo empty flag
  signal a2d_fifo_data_in : std_logic_vector(wordSize-1 downto 0);    -- fifo write data
  signal a2d_fifo_write_enable : std_logic;                   -- fifo write enable
  signal a2d_fifo_write_clear : std_logic;                    -- fifo write address clear
  signal a2d_fifo_data_out : std_logic_vector(wordSize-1 downto 0);   -- fifo read data
  signal a2d_fifo_read_enable : std_logic;                    -- fifo read enable
  signal a2d_fifo_read_clear : std_logic;                     -- fifo read address clear
  signal a2d_fifo_clear : std_logic;                          -- fifo clear signal
  signal a2d_fifo_read_flag : std_logic;                      -- fifo read flag
  signal a2d_read_address_inc : std_logic;                    -- read address gets incremented when set
  
  signal data_to_codec_buffer : std_logic_vector(wordSize-1 downto 0);  -- buffer to hold data to be sent to codec
  signal data_to_codec : std_logic_vector(wordSize-1 downto 0);         -- data to be sent to codec
  signal data_from_codec : std_logic_vector(wordSize-1 downto 0);       -- data read from the codec
  
  -- END SIGNALS
    
begin
  -- PORT MAPPING
  
  -- 
  -- Hook up A2D/D2A FIFOs
  --
  fifo_d2a : async_fifo
  port map (
    -- Reading port.
    Data_out    => d2a_fifo_data_out,
    Empty_out   => d2a_fifo_empty,
    ReadEn_in   => d2a_fifo_read_enable,
    RClk        => wm_clk_i,
    
    -- Writing port.
    Data_in     => d2a_fifo_data_in,
    Full_out    => d2a_fifo_full,
    WriteEn_in  => d2a_fifo_write_enable, 
    WClk        => wb_clk_i,

    Clear_in    => d2a_fifo_clear
  );

  fifo_a2d : async_fifo
  port map (
    -- Reading port.
    Data_out    => a2d_fifo_data_out,
    Empty_out   => a2d_fifo_empty,
    ReadEn_in   => a2d_fifo_read_enable,
    RClk        => wb_clk_i,
    
    -- Writing port.
    Data_in     => a2d_fifo_data_in,
    Full_out    => a2d_fifo_full,
    WriteEn_in  => a2d_fifo_write_enable, 
    WClk        => wm_clk_i,

    Clear_in    => a2d_fifo_clear
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
      d2a_fifo_write_clear <= '1';
      a2d_fifo_read_clear  <= '1';
    elsif rising_edge(wb_clk_i) then
      d2a_fifo_write_clear <= '0';
      a2d_fifo_read_clear  <= '0';
    end if;
  end process;

  process(wm_clk_i, wb_rst_i)
  begin
    if (wb_rst_i = '1') then
      d2a_fifo_read_clear  <= '1';
      a2d_fifo_write_clear <= '1';
    elsif rising_edge(wm_clk_i) then
      d2a_fifo_read_clear  <= '0';
      a2d_fifo_write_clear <= '0';
    end if;
  end process;
  
  d2a_fifo_clear <= d2a_fifo_write_clear or d2a_fifo_read_clear;
  a2d_fifo_clear <= a2d_fifo_write_clear or a2d_fifo_read_clear;
  
  -- 
  -- Write data to the D2A FIFO.
  --
  write_d2a_data: process(wb_rst_i, wb_clk_i)
  begin
    -- 
    -- Reset
    --
    if (wb_rst_i = '1') then
      d2a_fifo_write_enable <= '0';
      d2a_fifo_data_in <= (others => '0');
    
    -- 
    -- Prepare data for writing.
    --
    elsif rising_edge(wb_clk_i) then
      --
      -- Initialize
      --
      d2a_fifo_data_in <= (others => '0');
      d2a_fifo_write_enable <= '0';
      
      -- 
      -- Process the different addresses on write
      --
      if (wb_cyc_i='1' and wb_stb_i='1' and wb_we_i='1') then
        case wb_adr_i(minIObit+2 downto minIObit) is
          -- 
          -- write data register.
          --
          when "000" => 
            if (d2a_fifo_full = '0') then
              d2a_fifo_data_in <= wb_dat_i;
              d2a_fifo_write_enable <= '1';
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
            d2a_fifo_data_in <= (others => 'X');
            
        end case;
      end if;
    end if;
  end process;
  
  -- 
  -- Read data from the A2D FIFO
  -- Load the output data when address is read.
  --
  a2d_fifo_read_flag <= iack_o and not(wb_we_i);
  read_a2d_data: process(a2d_fifo_read_flag)
  begin
    if (a2d_fifo_read_flag = '0') then
      a2d_read_address_inc <= '0';
    else
      --
      -- Process the different addresses.
      --
      case wb_adr_i(minIObit+2 downto minIObit) is
        --
        -- Read data from the async fifo
        --
        when "000" =>
          wb_dat_o <= a2d_fifo_data_out;
          a2d_read_address_inc <= '1';
          
        -- 
        -- Read status flags.
        -- 
        when "001" =>
          wb_dat_o <= (others => '0');
          wb_dat_o(0) <= d2a_fifo_empty;
          wb_dat_o(1) <= d2a_fifo_full;
          wb_dat_o(2) <= a2d_fifo_empty;
          wb_dat_o(3) <= a2d_fifo_full;
          a2d_read_address_inc <= '0';
          
        --
        -- Illegal cases.
        --
        when others =>
          report ("Illegal read address, setting all values to unknown.");
          wb_dat_o <= (others => 'X');
          a2d_read_address_inc <= '0';
          
      end case;
    end if;
  end process;
  
  --
  -- Process to update the A2D FIFO read address.
  --
  process(wb_rst_i, wb_clk_i)
  begin
    if (wb_rst_i = '1') then
      a2d_fifo_read_enable <= '0';
    elsif rising_edge(wb_clk_i) then
      a2d_fifo_read_enable <= '0';
      if ((a2d_fifo_read_flag = '1') and (a2d_read_address_inc = '1')) then
        a2d_fifo_read_enable <= '1';
      end if;
    end if;
  end process;

  -- 
  -- Create the clocks used to drive the codec.
  -- The BCLK will be the codec clock / 2.
  -- The sample clock will be the codec clock / 256 but
  -- it's width has to match that of the BLCK.
  -- Created using an 8 bit counter.
  --
  process(wm_clk_i, wb_rst_i)
  begin
    if (wb_rst_i = '1') then
      codec_sample_clock_counter <= (others => '0');
    elsif rising_edge(wm_clk_i) then
      codec_sample_clock_counter <= codec_sample_clock_counter + 1;
    end if;
  end  process;
  
  codec_sample_clock <= and_reduce(std_logic_vector(
    codec_sample_clock_counter(7 downto 1)));
  codec_bclk <= std_logic(
    codec_sample_clock_counter(0));
    
  --
  -- Process to read data from the D2A FIFO to be sent to the codec.
  -- The clock produces a single hi pulse every 256 pulses.
  -- It is expected the chip will be configured in DSP mode and
  -- the data is to be sent to the chip immediately following that hi bit
  -- (see figure 29 in the WM8731 data sheet).
  --
  read_d2a_data: process(wm_clk_i, wb_rst_i)
  begin
    --
    -- Initialization
    --  
    if (wb_rst_i = '1') then
      data_to_codec_buffer <= (others => '0');
      data_to_codec <= (others => '0');
      d2a_fifo_read_enable <= '0';
      
    --
    -- Processing
    --
    elsif rising_edge(wm_clk_i) then
      --
      -- Initialize
      --
      d2a_fifo_read_enable <= '0';
      
      --
      -- Data shifts occur on the falling edge of the bclk, 
      -- which occurs when bclk = '1' on the rising edge
      -- of wm_clk_i
      -- 
      if (codec_bclk = '1') then
        --
        -- Load new data on the falling edge of the sample
        -- clock.  This is mode A of the WM8731.
        --
        if (codec_sample_clock = '1') then
          --
          -- Load data from the FIFO.  If the FIFO is not
          -- empty increment to the next value.
          --
          data_to_codec <= data_to_codec_buffer;        
          if (d2a_fifo_empty = '0') then
            data_to_codec_buffer <= d2a_fifo_data_out;
            d2a_fifo_read_enable <= '1';
          end if;
          
        --
        -- If you're not on the falling edge of the sample
        -- clock data has already been loaded and you just
        -- shift a new bit into place.
        --
        else
          data_to_codec <= data_to_codec(wordSize-2 downto 0) & '1';
        end if;
      end if;
    end if;
  end process;

  -- 
  -- Outgoing signals to the WM8731
  -- 
  wm_bclk_o <= codec_bclk;
  wm_lrc_o  <= codec_sample_clock;
  wm_dacdat_o <= data_to_codec(wordSize-1);

  --
  -- Process to read data from the codec to be sent to the A2D FIFO.
  --
  -- process(wm_rst_i, wm_clk_i)
  -- begin
    -- if (wm_rst_i = '1') then
      -- data_to_codec <= (others => '0');
    -- elsif rising_edge(wm_clk_i) then
      -- if (a2d_fifo_empty = '0') then
        -- data_to_codec <= d2a_fifo_data_out;
      -- end if;
    -- end if;
  -- end process;
      
  -- END PROCESSING

end architecture structural;

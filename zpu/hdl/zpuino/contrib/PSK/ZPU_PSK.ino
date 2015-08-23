/* Example Arduino sketch showing how to use the PSK31
 * block for the ZPUino.
 */
 
/* Define our Base IO address to slot 8
 */
#define PSK_SLOT     8

/* Define the PSK registers.
 */
#define PSK_BASE    IO_SLOT(PSK_SLOT)
#define PSK_DATA    REGISTER(PSK_BASE, 0)
#define DDS_DATA    REGISTER(PSK_BASE, 1)
#define CONTROL_REG REGISTER(PSK_BASE, 2)
#define STATUS_REG  REGISTER(PSK_BASE, 2)

/* Control values
 */
#define PSK_ENABLE  0x01

/* Status masks
 */
#define PSK_ENABLED     ((STATUS_REG & 0x01) != 0)
#define DATA_REG_EMPTY  ((STATUS_REG & 0x02) != 0)

/* Define PPS channels.
 */
#define PSK_I_CHANNEL  8
#define PSK_Q_CHANNEL  7
#define DEBUG_0        40

/* Assign FPGA pins to the signal lines.
 */
#define PSK_I_AUDIO  15
#define PSK_Q_AUDIO  14
#define DEBUG_PIN_0  15

/* Prototypes
 */
unsigned CalculateDDSInc(unsigned frequency_hz);

/* Code is stored as two values, the upper and lower bytes.
 * The varicode bits are left aligned, followed by the
 * 00 bits that separate varicode elements and one filled.
 * So for example, the NULL is stored as:
 * 1010101011_00_1111
 * (Varicode)_(zeros)_(fill)
 */
unsigned const varicode[256] =	{
    B10101010, B11001111, // 0 NUL
    B10110110, B11001111, // 1 SOH
    B10111011, B01001111, // 2 STX
    B11011101, B11001111, // 3 ETX
    B10111010, B11001111, // 4 EOT
    B11010111, B11001111, // 5 ENQ
    B10111011, B11001111, // 6 ACK
    B10111111, B01001111, // 7 BEL
    B10111111, B11001111, // 8 BS
    B11101111, B00111111, // 9 HT
    B11101001, B11111111, // 10 LF
    B11011011, B11001111, // 11 VT
    B10110111, B01001111, // 12 FF
    B11111001, B11111111, // 13 CR
    B11011101, B01001111, // 14 SO
    B11101010, B11001111, // 15 SI
    B10111101, B11001111, // 16 DLE
    B10111101, B01001111, // 17 DC1
    B11101011, B01001111, // 18 DC2
    B11101011, B11001111, // 19 DC3
    B11010110, B11001111, // 20 DC4
    B11011010, B11001111, // 21 NAK
    B11011011, B01001111, // 22 SYN
    B11010101, B11001111, // 23 ETB
    B11011110, B11001111, // 24 CAN
    B11011111, B01001111, // 25 EM
    B11101101, B11001111, // 26 SUB
    B11010101, B01001111, // 27 ESC
    B11010111, B01001111, // 28 FS
    B11101110, B11001111, // 29 GS
    B10111110, B11001111, // 30 RS
    B11011111, B11001111, // 31 US
    B10011111, B11111111, // 32 SP
    B11111111, B10011111, // 33 !
    B10101111, B10011111, // 34 "
    B11111010, B10011111, // 35 #
    B11101101, B10011111, // 36 $
    B10110101, B01001111, // 37 %
    B10101110, B11001111, // 38 &
    B10111111, B10011111, // 39 '
    B11111011, B00111111, // 40 (
    B11110111, B00111111, // 41 )
    B10110111, B10011111, // 42 *
    B11101111, B10011111, // 43 +
    B11101010, B00111111, // 44 ,
    B11010100, B11111111, // 45 -
    B10101110, B01111111, // 46 .
    B11010111, B10011111, // 47 /
    B10110111, B00111111, // 48 0
    B10111101, B00111111, // 49 1
    B11101101, B00111111, // 50 2
    B11111111, B00111111, // 51 3
    B10111011, B10011111, // 52 4
    B10101101, B10011111, // 53 5
    B10110101, B10011111, // 54 6
    B11010110, B10011111, // 55 7
    B11010101, B10011111, // 56 8
    B11011011, B10011111, // 57 9
    B11110101, B00111111, // 58 :
    B11011110, B10011111, // 59 ;
    B11110110, B10011111, // 60 <
    B10101010, B01111111, // 61 =
    B11101011, B10011111, // 62 >
    B10101011, B11001111, // 63 ?
    B10101111, B01001111, // 64 @
    B11111010, B01111111, // 65 A
    B11101011, B00111111, // 66 B
    B10101101, B00111111, // 67 C
    B10110101, B00111111, // 68 D
    B11101110, B01111111, // 69 E
    B11011011, B00111111, // 70 F
    B11111101, B00111111, // 71 G
    B10101010, B10011111, // 72 H
    B11111110, B01111111, // 73 I
    B11111110, B10011111, // 74 J
    B10111110, B10011111, // 75 K
    B11010111, B00111111, // 76 L
    B10111011, B00111111, // 77 M
    B11011101, B00111111, // 78 N
    B10101011, B00111111, // 79 O
    B11010101, B00111111, // 80 P
    B11101110, B10011111, // 81 Q
    B10101111, B00111111, // 82 R
    B11011110, B01111111, // 83 S
    B11011010, B01111111, // 84 T
    B10101011, B10011111, // 85 U
    B11011010, B10011111, // 86 V
    B10101110, B10011111, // 87 W
    B10111010, B10011111, // 88 X
    B10111101, B10011111, // 89 Y
    B10101011, B01001111, // 90 Z
    B11111011, B10011111, // 91 [
    B11110111, B10011111, // 92 \ (backslash)
    B11111101, B10011111, // 93 ]
    B10101111, B11001111, // 94 ^
    B10110110, B10011111, // 95 _ (underline)
    B10110111, B11001111, // 96 `
    B10110011, B11111111, // 97 a
    B10111110, B01111111, // 98 B
    B10111100, B11111111, // 99 c
    B10110100, B11111111, // 100 d
    B11001111, B11111111, // 101 e
    B11110100, B11111111, // 102 f
    B10110110, B01111111, // 103 g
    B10101100, B11111111, // 104 h
    B11010011, B11111111, // 105 i
    B11110101, B10011111, // 106 j
    B10111111, B00111111, // 107 k
    B11011001, B11111111, // 108 l
    B11101100, B11111111, // 109 m
    B11110011, B11111111, // 110 n
    B11100111, B11111111, // 111 o
    B11111100, B11111111, // 112 p
    B11011111, B10011111, // 113 q
    B10101001, B11111111, // 114 r
    B10111001, B11111111, // 115 s
    B10100111, B11111111, // 116 t
    B11011100, B11111111, // 117 u
    B11110110, B01111111, // 118 v
    B11010110, B01111111, // 119 w
    B11011111, B00111111, // 120 x
    B10111010, B01111111, // 121 y
    B11101010, B10011111, // 122 z
    B10101101, B11001111, // 123 {
    B11011101, B10011111, // 124 |
    B10101101, B01001111, // 125 }
    B10110101, B11001111, // 126 ~
    B11101101, B01001111, // 127 (del)
};

void setup()
{
    /* Set the pin modes.
     */
    pinMode(PSK_I_AUDIO, OUTPUT);
    pinModePPS(PSK_I_AUDIO, HIGH);
    outputPinForFunction(PSK_I_AUDIO, PSK_I_CHANNEL);
    
    pinMode(PSK_Q_AUDIO, OUTPUT);
    pinModePPS(PSK_Q_AUDIO, HIGH);
    outputPinForFunction(PSK_Q_AUDIO, PSK_Q_CHANNEL);

    /* Set the PSK frequency.
     */
    unsigned frequency_hz = 1000;       // DDS frequency in Hz.
    unsigned dds_increment = CalculateDDSInc(frequency_hz);
    
    Serial.begin(9600);
    
    DDS_DATA = dds_increment;
    CONTROL_REG = PSK_ENABLE;
}

void loop() 
{

    if (DATA_REG_EMPTY)
    {
        if (Serial.available() > 0)
        {
            int incomingByte = Serial.read();
            unsigned pskBeaconVaricode = 
                ((varicode[2*incomingByte+0] & 0x00FF) << 8) +
                ((varicode[2*incomingByte+1] & 0x00FF) << 0);
            PSK_DATA = pskBeaconVaricode;
        }
    }
}

unsigned CalculateDDSInc(unsigned frequency_hz)
{
    unsigned quotient = 0;
    unsigned acc = frequency_hz;
    
    for (int i = 0; i < 32; i++)
    {
        quotient = quotient << 1;
        acc = acc << 1;
        if (acc > 96000000)
        {
            quotient += 1;
            acc -= 96000000;
        }
    }
    return quotient;
}




library IEEE ;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity AXIS_UART is
    generic (
        CLK_frequency_Hz    : integer               := 100000000 ;
        Baud_Rate           : integer               := 9600 ;
        Data_Bits           : integer Range 5 to 8  := 8 ;
        Parity_Enable       : integer Range 0 to 1  := 0 ;
        Parity_Even0_Odd1   : integer Range 0 to 1  := 0 
    ) ;
    port (
        aclk                : in  std_logic ;
        aresetn             : in  std_logic ;
        uart_tx             : out std_logic ;
        uart_rx             : in  std_logic ;
        
        s_axis_tx_tdata     : in  std_logic_vector(7 downto 0) ;
        s_axis_tx_tvalid    : in  std_logic ;
        s_axis_tx_tready    : out std_logic ;
        
        m_axis_rx_tdata     : out std_logic_vector(7 downto 0) ;
        m_axis_rx_tvalid    : out std_logic ;
        m_axis_rx_tready    : in  std_logic
    );
end AXIS_UART;

architecture Behavioral of AXIS_UART is

    type tx_state_type is (s_ready, s_send) ;
    type rx_state_type is (s_falling_edge, s_start_bit, s_data_bits, s_stop_bit) ;
    
    constant    ratio           : integer                                               := CLK_frequency_Hz/Baud_Rate ;

    signal      tx_state        : tx_state_type                                         := s_ready ;
    signal      rx_state        : rx_state_type                                         := s_falling_edge ;
    signal      tx_baud_counter : integer range 0 to ratio-1                            := 0 ;
    signal      rx_baud_counter : integer range 0 to ratio-1                            := 0 ;
    signal      tx_bit_counter  : integer range 0 to Data_Bits+Parity_Enable+1          := 0 ;
    signal      rx_bit_counter  : integer range 0 to Data_Bits+Parity_Enable-1          := 0 ;
    signal      tx_bits         : std_logic_vector(Data_Bits+Parity_Enable+1 downto 0)  := (others=>'0') ;
    signal      rx_bits         : std_logic_vector(Data_Bits+Parity_Enable-1 downto 0)  := (others=>'0') ;
    signal      rx_data         : std_logic_vector(7                         downto 0)  := (others=>'0') ;
    signal      uart_rx_reg     : std_logic                                             := '0' ;
    signal      rx_valid        : std_logic                                             := '0' ;
    
    
    
begin
    
    uart_tx             <= tx_bits(0)   when tx_state=s_send    else '1' ;
    s_axis_tx_tready    <= '1'          when tx_state=s_ready   else '0' ; 
    
    tx_process : process(aclk) 
        variable    parity  :   std_logic := '0' ;
    begin
        if rising_edge(aclk) then 
            
            parity := '0' ;
            for i in 0 to Data_Bits-1 loop
                parity := parity xor s_axis_tx_tdata(i) ;
            end loop ;
            if Parity_Even0_Odd1=1 then
                parity := not parity ;
            end if ;
            
            if aresetn='0' then
            
                tx_state        <= s_ready ;
                tx_bits         <= (others=>'0') ;
                tx_baud_counter <= 0 ;
                tx_bit_counter  <= 0 ;
                
            else
            
                case tx_state is
                
                when s_ready =>
                    tx_baud_counter <= 0 ;
                    tx_bit_counter  <= 0 ;
                    if s_axis_tx_tvalid='1' then
                        if Parity_Enable=0 then
                            tx_bits <= '1' & s_axis_tx_tdata(Data_Bits-1 downto 0) & '0' ;
                        else
                            tx_bits <= '1' & parity & s_axis_tx_tdata(Data_Bits-1 downto 0) & '0' ;
                        end if ;
                        tx_state    <= s_send ;
                    end if ;
      
                when s_send =>
                    if tx_baud_counter=ratio-1 then
                        tx_baud_counter <= 0 ;
                        if tx_bit_counter=Data_Bits+Parity_Enable+1 then
                            tx_bit_counter  <= 0 ;
                            tx_state        <= s_ready ;
                        else
                            tx_bit_counter  <= tx_bit_counter + 1 ;
                        end if ;
                        tx_bits <= '0' & tx_bits(tx_bits'High downto 1) ;
                    else
                        tx_baud_counter <= tx_baud_counter + 1 ;
                    end if ;

                end case ;
            end if ;
        end if ;
    end process ;


    rx_process : process(aclk) 
        variable    parity  :   std_logic := '0' ;
    begin
        if rising_edge(aclk) then
            
            parity := '0' ;
            for i in 0 to Data_Bits-1 loop
                parity := parity xor rx_bits(i) ;
            end loop ;
            if Parity_Even0_Odd1=1 then
                parity := not parity ;
            end if ;
            
            
            if aresetn = '0' then
            
                rx_state        <= s_falling_edge ;
                uart_rx_reg     <= '0' ;
                rx_baud_counter <= 0 ;
                rx_bit_counter  <= 0 ;
                rx_valid        <= '0' ;
                rx_data         <= (others=>'0') ;
            else
                
                if rx_valid='1' and m_axis_rx_tready='1' then
                    rx_valid    <= '0' ;
                end if;
            
                uart_rx_reg <= uart_rx ;
                
                case rx_state is
                when s_falling_edge =>
                    if uart_rx_reg='1' and uart_rx='0' then  
                        rx_state    <= s_start_bit ;
                    end if ;
      
                when s_start_bit =>
                    if rx_baud_counter=ratio/2-1 then
                        rx_baud_counter <= 0 ;
                        if uart_rx='0' then
                            rx_state    <= s_data_bits ;
                        else
                            rx_state    <= s_falling_edge ;
                        end if ;
                    else
                        rx_baud_counter <= rx_baud_counter + 1 ;
                    end if ;
      
                when s_data_bits =>
                    if rx_baud_counter=ratio-1 then
                        rx_baud_counter <= 0 ;
                        if rx_bit_counter=Data_Bits+Parity_Enable-1 then
                            rx_bit_counter  <= 0 ;
                            rx_state        <= s_stop_bit ;
                        else
                            rx_bit_counter  <= rx_bit_counter + 1 ;
                        end if ;
                        rx_bits <= uart_rx & rx_bits(rx_bits'High downto 1) ;
                    else
                        rx_baud_counter <= rx_baud_counter + 1 ;
                    end if ;

      
                when s_stop_bit =>
                    if rx_baud_counter=ratio-1 then
                        rx_baud_counter <= 0 ;
                        rx_state        <= s_falling_edge ;
                        if uart_rx='1' then
                            rx_data(Data_Bits-1 downto 0)   <= rx_bits(Data_Bits-1 downto 0) ;
                            if Parity_Enable=0 then
                                rx_valid                        <= '1' ;
                            elsif rx_bits(Data_Bits)=Parity then
                                rx_valid                        <= '1' ;
                            end if;
                        end if ;
                    else
                        rx_baud_counter <= rx_baud_counter + 1 ;
                    end if ;
                        
                end case ;
                
            end if ;
        end if ;
    end process ;

    m_axis_rx_tdata     <= rx_data ;
    m_axis_rx_tvalid    <= rx_valid ;

end Behavioral ;
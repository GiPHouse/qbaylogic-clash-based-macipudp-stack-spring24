`default_nettype none
`timescale 100fs/100fs
module udpEcho
    ( // Inputs
      input wire  clk25 // clock
    // , input wire  uart_rx
    // , inout wire [31:0] sdram_dq
    // , inout wire [0:0] eth_mdio
    , input wire  eth0_rx_clk // clock
    , input wire  eth0_rx_ctl
    , input wire [3:0] eth0_rx_data
    // , input wire  eth1_rx_clk // clock
    // , input wire  eth1_rx_ctl
    // , input wire [3:0] eth1_rx_data

      // Outputs
    // , output wire  uart_tx
    // , output wire  sdram_clk // clock
    // , output wire [10:0] sdram_a
    // , output wire  sdram_we_n
    // , output wire  sdram_ras_n
    // , output wire  sdram_cas_n
    // , output wire [1:0] sdram_ba
    // , output wire  eth_mdc
    , output wire  eth0_tx_clk
    , output wire  eth0_tx_ctl
    , output wire [3:0] eth0_tx_data
    // , output wire  eth1_tx_clk
    // , output wire  eth1_tx_ctl
    // , output wire [3:0] eth1_tx_data
    // , output wire  hub_clk
    // , output wire [4:0] hub_line_select
    // , output wire  hub_latch
    // , output wire  hub_output_enable
    // , output wire [47:0] hub_data
    );

  wire clk125;
  wire clk125_locked;
  wire clk125_nlocked;
  wire rst125;

  wire clk50;
  wire clk50_locked;
  wire clk50_nlocked;
  wire rst50;

  reg rst50_sync0;
  reg rst50_sync1;

  reg rst125_sync0;
  reg rst125_sync1;

  // PLL for 125Mhz clock
  (* FREQUENCY_PIN_CLKI="25" *)
  (* FREQUENCY_PIN_CLKOP="125" *)
  (* ICP_CURRENT="12" *) (* LPF_RESISTOR="8" *) (* MFG_ENABLE_FILTEROPAMP="1" *) (* MFG_GMCREF_SEL="2" *)
  EHXPLLL #(
        .PLLRST_ENA("DISABLED"),
        .INTFB_WAKE("DISABLED"),
        .STDBY_ENABLE("DISABLED"),
        .DPHASE_SOURCE("DISABLED"),
        .OUTDIVIDER_MUXA("DIVA"),
        .OUTDIVIDER_MUXB("DIVB"),
        .OUTDIVIDER_MUXC("DIVC"),
        .OUTDIVIDER_MUXD("DIVD"),
        .CLKI_DIV(1),
        .CLKOP_ENABLE("ENABLED"),
        .CLKOP_DIV(5),
        .CLKOP_CPHASE(2),
        .CLKOP_FPHASE(0),
        .FEEDBK_PATH("CLKOP"),
        .CLKFB_DIV(5)
    ) pll125_inst (
        .RST(1'b0),
        .STDBY(1'b0),
        .CLKI(clk25),
        .CLKOP(clk125),
        .CLKFB(clk125),
        .CLKINTFB(),
        .PHASESEL0(1'b0),
        .PHASESEL1(1'b0),
        .PHASEDIR(1'b1),
        .PHASESTEP(1'b1),
        .PHASELOADREG(1'b1),
        .PLLWAKESYNC(1'b0),
        .ENCLKOP(1'b0),
        .LOCK(clk125_locked)
    );

    // Sync locked signal as reset
    assign clk125_nlocked = ~clk125_locked;
    assign rst125 = rst125_sync1;
    always @(posedge clk125 or posedge clk125_nlocked) begin
        if (clk125_nlocked) begin
            rst125_sync0 <= 1'b1;
            rst125_sync1 <= 1'b1;
        end else begin
            rst125_sync0 <= 1'b0;
            rst125_sync1 <= rst125_sync0;
        end
    end

    // 50Mhz clock for logic
    (* FREQUENCY_PIN_CLKI="25" *)
    (* FREQUENCY_PIN_CLKOP="50" *)
    (* ICP_CURRENT="12" *) (* LPF_RESISTOR="8" *) (* MFG_ENABLE_FILTEROPAMP="1" *) (* MFG_GMCREF_SEL="2" *)
    EHXPLLL #(
      .PLLRST_ENA("DISABLED"),
      .INTFB_WAKE("DISABLED"),
      .STDBY_ENABLE("DISABLED"),
      .DPHASE_SOURCE("DISABLED"),
      .OUTDIVIDER_MUXA("DIVA"),
      .OUTDIVIDER_MUXB("DIVB"),
      .OUTDIVIDER_MUXC("DIVC"),
      .OUTDIVIDER_MUXD("DIVD"),
      .CLKI_DIV(1),
      .CLKOP_ENABLE("ENABLED"),
      .CLKOP_DIV(12),
      .CLKOP_CPHASE(5),
      .CLKOP_FPHASE(0),
      .FEEDBK_PATH("CLKOP"),
      .CLKFB_DIV(2)
    ) pll50_inst (
      .RST(1'b0),
      .STDBY(1'b0),
      .CLKI(clk25),
      .CLKOP(clk50),
      .CLKFB(clk50),
      .CLKINTFB(),
      .PHASESEL0(1'b0),
      .PHASESEL1(1'b0),
      .PHASEDIR(1'b1),
      .PHASESTEP(1'b1),
      .PHASELOADREG(1'b1),
      .PLLWAKESYNC(1'b0),
      .ENCLKOP(1'b0),
      .LOCK(clk50_locked)
  	);

    // Sync locked signal as reset
    assign clk50_nlocked = ~clk50_locked;
    assign rst50 = rst50_sync1;
    always @(posedge clk50 or posedge clk50_nlocked) begin
        if (clk50_nlocked) begin
            rst50_sync0 <= 1'b1;
            rst50_sync1 <= 1'b1;
        end else begin
            rst50_sync0 <= 1'b0;
            rst50_sync1 <= rst50_sync0;
        end
    end

    // udp core signals
    // We do nothing else but echo back incoming packets with src/dst ports swapped
    wire udp_fwd_valid;
    wire [31:0] udp_fwd_data;
    wire [1:0] udp_fwd_last;
    wire udp_fwd_last_valid;
    wire udp_fwd_abort;
    wire [31:0] udp_fwd_ip;
    wire [15:0] udp_fwd_src_port;
    wire [15:0] udp_fwd_dst_port;
    wire [15:0] udp_fwd_length;
    wire  udp_ready;

    udpCore udpCore_inst (
        // Clocks and resets
        .clk50(clk50),
        .rst50(rst50),
        .eth_tx_clk_in(clk125),
        .eth_tx_rst(rst125),

        // Phy eth rx
        .eth_rx_clk(eth0_rx_clk),
        .eth_rx_ctl(eth0_rx_ctl),
        .eth_rx_data(eth0_rx_data),

        // ethernet config
        // Mac adress
        .mac(48'hAE0000000000),
        // 192.168.100.1
        .ip(32'hC0A86401),
        // 255.255.255.0
        .subnetmask(32'hFFFFFF00),

        // UDP packet to transmit out
        // If any abort is set during a packet the packet is dropped
        // backpressure follows axi stream valid/ready handshake
        .udp_in_fwd_valid(udp_fwd_valid),
        .udp_in_fwd_data(udp_fwd_data),
        .udp_in_fwd_last(udp_fwd_last),
        .udp_in_fwd_last_valid(udp_fwd_last_valid),
        .udp_in_fwd_abort(udp_fwd_abort),
        .udp_in_fwd_ip(udp_fwd_ip),
        .udp_in_fwd_src_port(udp_fwd_src_port),
        .udp_in_fwd_dst_port(udp_fwd_dst_port),
        .udp_in_fwd_length(udp_fwd_length),
        .udp_out_ready(udp_ready),

        // Outputs
        // Phy eth tx
        .eth_tx_clk(eth0_tx_clk),
        .eth_tx_ctl(eth0_tx_ctl),
        .eth_tx_data(eth0_tx_data),

        // UDP packets received
        .udp_out_fwd_valid(udp_fwd_valid),
        .udp_out_fwd_data(udp_fwd_data),
        .udp_out_fwd_last(udp_fwd_last),
        .udp_out_fwd_last_valid(udp_fwd_last_valid),
        .udp_out_fwd_abort(udp_fwd_abort),
        .udp_out_fwd_ip(udp_fwd_ip),
        .udp_out_fwd_src_port(udp_fwd_dst_port),
        .udp_out_fwd_dst_port(udp_fwd_src_port),
        .udp_out_fwd_length(udp_fwd_length),
        .udp_in_ready(udp_ready)
    );

endmodule

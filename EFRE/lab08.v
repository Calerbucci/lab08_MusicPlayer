`define C 4'd0
`define D 4'd1
`define E 4'd2
`define F 4'd3
`define G 4'd4
`define A 4'd5
`define B 4'd6
`define min 4'd7
`define hc  32'd524
`define hd  32'd588
`define he  32'd660
`define hf  32'd698
`define hg  32'd784
`define ha  32'd880
`define hb  32'd988 
`define c   32'd262   
`define d   32'd294 
`define e   32'd330 
`define f   32'd349 
`define g   32'd392 
`define gs  32'd415
`define a   32'd440
`define b   32'd494
`define sil   32'd50000000 // slience
`define SONG1 511
`define SONG2 1055

module clock_divider(clk, clk_div);   
    parameter n = 26;     
    input clk;   
    output clk_div;   
    
    reg [n-1:0] num;
    wire [n-1:0] next_num;
    
    always@(posedge clk)begin
    	num<=next_num;
    end
    
    assign next_num = num +1;
    assign clk_div = num[n-1];
    
endmodule

module debounce(pb_debounced, pb ,clk);
    output pb_debounced;
    input pb;
    input clk;
    
    reg [6:0] shift_reg;
    always @(posedge clk) begin
        shift_reg[6:1] <= shift_reg[5:0];
        shift_reg[0] <= pb;
    end
    
    assign pb_debounced = shift_reg == 7'b111_1111 ? 1'b1 : 1'b0;
endmodule

module onepulse(signal, clk, op);
    input signal, clk;
    output op;
    
    reg op;
    reg delay;
    
    always @(posedge clk) begin
        if((signal == 1) & (delay == 0)) op <= 1;
        else op <= 0; 
        delay = signal;
    end
endmodule

module _player_control (
	input clk,
	input reset,
	input _play,
	input _repeat,
	input _music,
	output reg [11:0] ibeat
);
    reg last_music;
    
    always @(posedge clk or posedge reset) begin
        if(_music == 0) begin
            if(reset || last_music) begin
                ibeat = 0;
                last_music = 0;
            end
            if(_play) begin
                if(ibeat < `SONG1) begin
                    ibeat = ibeat + 1;
                end
                else if(ibeat >= `SONG1) begin
                    if(_repeat) begin
                        ibeat = 0;
                    end
                    else begin
                        ibeat = 4095;
                    end
                end
            end
            else begin
                ibeat = ibeat;
            end
        end 
        else begin
            if(reset || last_music) begin
                ibeat = 512;
                last_music = 1;
            end
            if(_play) begin
                if(ibeat < `SONG2) begin
                    ibeat = ibeat + 1;
                end
                else if(ibeat >= `SONG2) begin
                    if(_repeat) begin
                        ibeat = 512;
                    end
                    else begin
                        ibeat = 4095;
                    end
                end
            end
        end  
    end  
endmodule

module music_example (
	input [11:0] ibeatNum,
	input en,
	output reg [31:0] toneL,
    output reg [31:0] toneR
);

    always @* begin
        if(en == 0) begin
            case(ibeatNum)
                // --- Measure 1 ---
                12'd0: toneR = `hg;      12'd1: toneR = `hg; // HG (half-beat)
                12'd2: toneR = `hg;      12'd3: toneR = `hg;
                12'd4: toneR = `hg;      12'd5: toneR = `hg;
                12'd6: toneR = `hg;      12'd7: toneR = `hg;
                12'd8: toneR = `he;      12'd9: toneR = `he; // HE (half-beat)
                12'd10: toneR = `he;     12'd11: toneR = `he;
                12'd12: toneR = `he;     12'd13: toneR = `he;
                12'd14: toneR = `he;     12'd15: toneR = `sil; // (Short break for repetitive notes: high E)

                12'd16: toneR = `he;     12'd17: toneR = `he; // HE (one-beat)
                12'd18: toneR = `he;     12'd19: toneR = `he;
                12'd20: toneR = `he;     12'd21: toneR = `he;
                12'd22: toneR = `he;     12'd23: toneR = `he;
                12'd24: toneR = `he;     12'd25: toneR = `he;
                12'd26: toneR = `he;     12'd27: toneR = `he;
                12'd28: toneR = `he;     12'd29: toneR = `he;
                12'd30: toneR = `he;     12'd31: toneR = `he;

                12'd32: toneR = `hf;     12'd33: toneR = `hf; // HF (half-beat)
                12'd34: toneR = `hf;     12'd35: toneR = `hf;
                12'd36: toneR = `hf;     12'd37: toneR = `hf;
                12'd38: toneR = `hf;     12'd39: toneR = `hf;
                12'd40: toneR = `hd;     12'd41: toneR = `hd; // HD (half-beat)
                12'd42: toneR = `hd;     12'd43: toneR = `hd;
                12'd44: toneR = `hd;     12'd45: toneR = `hd;
                12'd46: toneR = `hd;     12'd47: toneR = `sil; // (Short break for repetitive notes: high D)

                12'd48: toneR = `hd;     12'd49: toneR = `hd; // HD (one-beat)
                12'd50: toneR = `hd;     12'd51: toneR = `hd;
                12'd52: toneR = `hd;     12'd53: toneR = `hd;
                12'd54: toneR = `hd;     12'd55: toneR = `hd;
                12'd56: toneR = `hd;     12'd57: toneR = `hd;
                12'd58: toneR = `hd;     12'd59: toneR = `hd;
                12'd60: toneR = `hd;     12'd61: toneR = `hd;
                12'd62: toneR = `hd;     12'd63: toneR = `hd;

                // --- Measure 2 ---
                12'd64: toneR = `hc;     12'd65: toneR = `hc; // HC (half-beat)
                12'd66: toneR = `hc;     12'd67: toneR = `hc;
                12'd68: toneR = `hc;     12'd69: toneR = `hc;
                12'd70: toneR = `hc;     12'd71: toneR = `hc;
                12'd72: toneR = `hd;     12'd73: toneR = `hd; // HD (half-beat)
                12'd74: toneR = `hd;     12'd75: toneR = `hd;
                12'd76: toneR = `hd;     12'd77: toneR = `hd;
                12'd78: toneR = `hd;     12'd79: toneR = `hd;

                12'd80: toneR = `he;     12'd81: toneR = `he; // HE (half-beat)
                12'd82: toneR = `he;     12'd83: toneR = `he;
                12'd84: toneR = `he;     12'd85: toneR = `he;
                12'd86: toneR = `he;     12'd87: toneR = `he;
                12'd88: toneR = `hf;     12'd89: toneR = `hf; // HF (half-beat)
                12'd90: toneR = `hf;     12'd91: toneR = `hf;
                12'd92: toneR = `hf;     12'd93: toneR = `hf;
                12'd94: toneR = `hf;     12'd95: toneR = `hf;

                12'd96: toneR = `hg;     12'd97: toneR = `hg; // HG (half-beat)
                12'd98: toneR = `hg;     12'd99: toneR = `hg;
                12'd100: toneR = `hg;     12'd101: toneR = `hg;
                12'd102: toneR = `hg;     12'd103: toneR = `sil; // (Short break for repetitive notes: high D)
                12'd104: toneR = `hg;     12'd105: toneR = `hg; // HG (half-beat)
                12'd106: toneR = `hg;     12'd107: toneR = `hg;
                12'd108: toneR = `hg;     12'd109: toneR = `hg;
                12'd110: toneR = `hg;     12'd111: toneR = `sil; // (Short break for repetitive notes: high D)

                12'd112: toneR = `hg;     12'd113: toneR = `hg; // HG (one-beat)
                12'd114: toneR = `hg;     12'd115: toneR = `hg;
                12'd116: toneR = `hg;     12'd117: toneR = `hg;
                12'd118: toneR = `hg;     12'd119: toneR = `hg;
                12'd120: toneR = `hg;     12'd121: toneR = `hg;
                12'd122: toneR = `hg;     12'd123: toneR = `hg;
                12'd124: toneR = `hg;     12'd125: toneR = `hg;
                12'd126: toneR = `hg;     12'd127: toneR = `sil;
                
                // --- Measure 3 ---
				12'd128: toneR = `hg;			12'd129: toneR = `hg;
				12'd130: toneR = `hg;			12'd131: toneR = `hg;
				12'd132: toneR = `hg;			12'd133: toneR = `hg;
				12'd134: toneR = `hg;			12'd135: toneR = `hg;
				12'd136: toneR = `he;			12'd137: toneR = `he;
				12'd138: toneR = `he;			12'd139: toneR = `he;
				12'd140: toneR = `he;			12'd141: toneR = `he;
				12'd142: toneR = `he;			12'd143: toneR = `sil;

				12'd144: toneR = `he;			12'd145: toneR = `he;
				12'd146: toneR = `he;			12'd147: toneR = `he;
				12'd148: toneR = `he;			12'd149: toneR = `he;
				12'd150: toneR = `he;			12'd151: toneR = `he;
				12'd152: toneR = `he;			12'd153: toneR = `he;
				12'd154: toneR = `he;			12'd155: toneR = `he;
				12'd156: toneR = `he;			12'd157: toneR = `he;
				12'd158: toneR = `he;			12'd159: toneR = `he;

				12'd160: toneR = `hf;			12'd161: toneR = `hf;
				12'd162: toneR = `hf;			12'd163: toneR = `hf;
				12'd164: toneR = `hf;			12'd165: toneR = `hf;
				12'd166: toneR = `hf;			12'd167: toneR = `hf;
				12'd168: toneR = `hd;			12'd169: toneR = `hd;
				12'd170: toneR = `hd;			12'd171: toneR = `hd;
				12'd172: toneR = `hd;			12'd173: toneR = `hd;
				12'd174: toneR = `hd;			12'd175: toneR = `sil;

				12'd176: toneR = `hd;			12'd177: toneR = `hd;
				12'd178: toneR = `hd;			12'd179: toneR = `hd;
				12'd180: toneR = `hd;			12'd181: toneR = `hd;
				12'd182: toneR = `hd;			12'd183: toneR = `hd;
				12'd184: toneR = `hd;			12'd185: toneR = `hd;
				12'd186: toneR = `hd;			12'd187: toneR = `hd;
				12'd188: toneR = `hd;			12'd189: toneR = `hd;
				12'd190: toneR = `hd;			12'd191: toneR = `hd;

				// --- Measure 4 ---
				12'd192: toneR = `hc;			12'd193: toneR = `hc;
				12'd194: toneR = `hc;			12'd195: toneR = `hc;
				12'd196: toneR = `hc;			12'd197: toneR = `hc;
				12'd198: toneR = `hc;			12'd199: toneR = `hc;
				12'd200: toneR = `he;			12'd201: toneR = `he;
				12'd202: toneR = `he;			12'd203: toneR = `he;
				12'd204: toneR = `he;			12'd205: toneR = `he;
				12'd206: toneR = `he;			12'd207: toneR = `he;

				12'd208: toneR = `hg;			12'd209: toneR = `hg;
				12'd210: toneR = `hg;			12'd211: toneR = `hg;
				12'd212: toneR = `hg;			12'd213: toneR = `hg;
				12'd214: toneR = `hg;			12'd215: toneR = `sil;
				12'd216: toneR = `hg;			12'd217: toneR = `hg;
				12'd218: toneR = `hg;			12'd219: toneR = `hg;
				12'd220: toneR = `hg;			12'd221: toneR = `hg;
				12'd222: toneR = `hg;			12'd223: toneR = `hg;

				12'd224: toneR = `he;			12'd225: toneR = `he;
				12'd226: toneR = `he;			12'd227: toneR = `he;
				12'd228: toneR = `he;			12'd229: toneR = `he;
				12'd230: toneR = `he;			12'd231: toneR = `he;
				12'd232: toneR = `he;			12'd233: toneR = `he;
				12'd234: toneR = `he;			12'd235: toneR = `he;
				12'd236: toneR = `he;			12'd237: toneR = `he;
				12'd238: toneR = `he;			12'd239: toneR = `he;

				12'd240: toneR = `he;			12'd241: toneR = `he;
				12'd242: toneR = `he;			12'd243: toneR = `he;
				12'd244: toneR = `he;			12'd245: toneR = `he;
				12'd246: toneR = `he;			12'd247: toneR = `he;
				12'd248: toneR = `he;			12'd249: toneR = `he;
				12'd250: toneR = `he;			12'd251: toneR = `he;
				12'd252: toneR = `he;			12'd253: toneR = `he;
				12'd254: toneR = `he;			12'd255: toneR = `he;

				// --- Measure 5 ---
				12'd256: toneR = `hd;			12'd257: toneR = `hd;
				12'd258: toneR = `hd;			12'd259: toneR = `hd;
				12'd260: toneR = `hd;			12'd261: toneR = `hd;
				12'd262: toneR = `hd;			12'd263: toneR = `sil;
				12'd264: toneR = `hd;			12'd265: toneR = `hd;
				12'd266: toneR = `hd;			12'd267: toneR = `hd;
				12'd268: toneR = `hd;			12'd269: toneR = `hd;
				12'd270: toneR = `hd;			12'd271: toneR = `sil;

				12'd272: toneR = `hd;			12'd273: toneR = `hd;
				12'd274: toneR = `hd;			12'd275: toneR = `hd;
				12'd276: toneR = `hd;			12'd277: toneR = `hd;
				12'd278: toneR = `hd;			12'd279: toneR = `sil;
				12'd280: toneR = `hd;			12'd281: toneR = `hd;
				12'd282: toneR = `hd;			12'd283: toneR = `hd;
				12'd284: toneR = `hd;			12'd285: toneR = `hd;
				12'd286: toneR = `hd;			12'd287: toneR = `sil;

				12'd288: toneR = `hd;			12'd289: toneR = `hd;
				12'd290: toneR = `hd;			12'd291: toneR = `hd;
				12'd292: toneR = `hd;			12'd293: toneR = `hd;
				12'd294: toneR = `hd;			12'd295: toneR = `hd;
				12'd296: toneR = `he;			12'd297: toneR = `he;
				12'd298: toneR = `he;			12'd299: toneR = `he;
				12'd300: toneR = `he;			12'd301: toneR = `he;
				12'd302: toneR = `he;			12'd303: toneR = `he;

				12'd304: toneR = `hf;			12'd305: toneR = `hf;
				12'd306: toneR = `hf;			12'd307: toneR = `hf;
				12'd308: toneR = `hf;			12'd309: toneR = `hf;
				12'd310: toneR = `hf;			12'd311: toneR = `hf;
				12'd312: toneR = `hf;			12'd313: toneR = `hf;
				12'd314: toneR = `hf;			12'd315: toneR = `hf;
				12'd316: toneR = `hf;			12'd317: toneR = `hf;
				12'd318: toneR = `hf;			12'd319: toneR = `hf;

				// --- Measure 6 ---
				12'd320: toneR = `he;			12'd321: toneR = `he;
				12'd322: toneR = `he;			12'd323: toneR = `he;
				12'd324: toneR = `he;			12'd325: toneR = `he;
				12'd326: toneR = `he;			12'd327: toneR = `sil;
				12'd328: toneR = `he;			12'd329: toneR = `he;
				12'd330: toneR = `he;			12'd331: toneR = `he;
				12'd332: toneR = `he;			12'd333: toneR = `he;
				12'd334: toneR = `he;			12'd335: toneR = `sil;

				12'd336: toneR = `he;			12'd337: toneR = `he;
				12'd338: toneR = `he;			12'd339: toneR = `he;
				12'd340: toneR = `he;			12'd341: toneR = `he;
				12'd342: toneR = `he;			12'd343: toneR = `sil;
				12'd344: toneR = `he;			12'd345: toneR = `he;
				12'd346: toneR = `he;			12'd347: toneR = `he;
				12'd348: toneR = `he;			12'd349: toneR = `he;
				12'd350: toneR = `he;			12'd351: toneR = `sil;

				12'd352: toneR = `he;			12'd353: toneR = `he;
				12'd354: toneR = `he;			12'd355: toneR = `he;
				12'd356: toneR = `he;			12'd357: toneR = `he;
				12'd358: toneR = `he;			12'd359: toneR = `he;
				12'd360: toneR = `hf;			12'd361: toneR = `hf;
				12'd362: toneR = `hf;			12'd363: toneR = `hf;
				12'd364: toneR = `hf;			12'd365: toneR = `hf;
				12'd366: toneR = `hf;			12'd367: toneR = `hf;

				12'd368: toneR = `hg;			12'd369: toneR = `hg;
				12'd370: toneR = `hg;			12'd371: toneR = `hg;
				12'd372: toneR = `hg;			12'd373: toneR = `hg;
				12'd374: toneR = `hg;			12'd375: toneR = `hg;
				12'd376: toneR = `hg;			12'd377: toneR = `hg;
				12'd378: toneR = `hg;			12'd379: toneR = `hg;
				12'd380: toneR = `hg;			12'd381: toneR = `hg;
				12'd382: toneR = `hg;			12'd383: toneR = `hg;

				// --- Measure 7 ---
				12'd384: toneR = `hg;			12'd385: toneR = `hg;
				12'd386: toneR = `hg;			12'd387: toneR = `hg;
				12'd388: toneR = `hg;			12'd389: toneR = `hg;
				12'd390: toneR = `hg;			12'd391: toneR = `hg;
				12'd392: toneR = `he;			12'd393: toneR = `he;
				12'd394: toneR = `he;			12'd395: toneR = `he;
				12'd396: toneR = `he;			12'd397: toneR = `he;
				12'd398: toneR = `he;			12'd399: toneR = `sil;

				12'd400: toneR = `he;			12'd401: toneR = `he;
				12'd402: toneR = `he;			12'd403: toneR = `he;
				12'd404: toneR = `he;			12'd405: toneR = `he;
				12'd406: toneR = `he;			12'd407: toneR = `he;
				12'd408: toneR = `he;			12'd409: toneR = `he;
				12'd410: toneR = `he;			12'd411: toneR = `he;
				12'd412: toneR = `he;			12'd413: toneR = `he;
				12'd414: toneR = `he;			12'd415: toneR = `he;

				12'd416: toneR = `hf;			12'd417: toneR = `hf;
				12'd418: toneR = `hf;			12'd419: toneR = `hf;
				12'd420: toneR = `hf;			12'd421: toneR = `hf;
				12'd422: toneR = `hf;			12'd423: toneR = `hf;
				12'd424: toneR = `hd;			12'd425: toneR = `hd;
				12'd426: toneR = `hd;			12'd427: toneR = `hd;
				12'd428: toneR = `hd;			12'd429: toneR = `hd;
				12'd430: toneR = `hd;			12'd431: toneR = `sil;

				12'd432: toneR = `hd;			12'd433: toneR = `hd;
				12'd434: toneR = `hd;			12'd435: toneR = `hd;
				12'd436: toneR = `hd;			12'd437: toneR = `hd;
				12'd438: toneR = `hd;			12'd439: toneR = `hd;
				12'd440: toneR = `hd;			12'd441: toneR = `hd;
				12'd442: toneR = `hd;			12'd443: toneR = `hd;
				12'd444: toneR = `hd;			12'd445: toneR = `hd;
				12'd446: toneR = `hd;			12'd447: toneR = `hd;

				// --- Measure 8 ---
				12'd448: toneR = `hc;			12'd449: toneR = `hc;
				12'd450: toneR = `hc;			12'd451: toneR = `hc;
				12'd452: toneR = `hc;			12'd453: toneR = `hc;
				12'd454: toneR = `hc;			12'd455: toneR = `hc;
                12'd456: toneR = `he;			12'd457: toneR = `he;
				12'd458: toneR = `he;			12'd459: toneR = `he;
				12'd460: toneR = `he;			12'd461: toneR = `he;
				12'd462: toneR = `he;			12'd463: toneR = `he;

				12'd464: toneR = `hg;			12'd465: toneR = `hg;
				12'd466: toneR = `hg;			12'd467: toneR = `hg;
				12'd468: toneR = `hg;			12'd469: toneR = `hg;
				12'd470: toneR = `hg;			12'd471: toneR = `sil;
				12'd472: toneR = `hg;			12'd473: toneR = `hg;
				12'd474: toneR = `hg;			12'd475: toneR = `hg;
				12'd476: toneR = `hg;			12'd477: toneR = `hg;
				12'd478: toneR = `hg;			12'd479: toneR = `hg;

				12'd480: toneR = `hc;			12'd481: toneR = `hc;
				12'd482: toneR = `hc;			12'd483: toneR = `hc;
				12'd484: toneR = `hc;			12'd485: toneR = `hc;
				12'd486: toneR = `hc;			12'd487: toneR = `hc;
				12'd488: toneR = `hc;			12'd489: toneR = `hc;
				12'd490: toneR = `hc;			12'd491: toneR = `hc;
				12'd492: toneR = `hc;			12'd493: toneR = `hc;
				12'd494: toneR = `hc;			12'd495: toneR = `hc;

				12'd496: toneR = `hc;			12'd497: toneR = `hc;
				12'd498: toneR = `hc;			12'd499: toneR = `hc;
				12'd500: toneR = `hc;			12'd501: toneR = `hc;
				12'd502: toneR = `hc;			12'd503: toneR = `hc;
				12'd504: toneR = `hc;			12'd505: toneR = `hc;
				12'd506: toneR = `hc;			12'd507: toneR = `hc;
				12'd508: toneR = `hc;			12'd509: toneR = `hc;
				12'd510: toneR = `hc;			12'd511: toneR = `hc;
				
				12'd512: toneR = `sil;			12'd513: toneR = `sil;
				12'd514: toneR = `sil;			12'd515: toneR = `sil;
				12'd516: toneR = `sil;			12'd517: toneR = `sil;
				12'd518: toneR = `sil;			12'd519: toneR = `sil;
				12'd520: toneR = `sil;			12'd521: toneR = `sil;
				12'd522: toneR = `sil;			12'd523: toneR = `sil;
				12'd524: toneR = `sil;			12'd525: toneR = `sil;
				12'd526: toneR = `sil;			12'd527: toneR = `sil;

				12'd528: toneR = `sil;			12'd529: toneR = `sil;
				12'd530: toneR = `sil;			12'd531: toneR = `sil;
				12'd532: toneR = `sil;			12'd533: toneR = `sil;
				12'd534: toneR = `sil;			12'd535: toneR = `sil;
				12'd536: toneR = `sil;			12'd537: toneR = `sil;
				12'd538: toneR = `sil;			12'd539: toneR = `sil;
				12'd540: toneR = `sil;			12'd541: toneR = `sil;
				12'd542: toneR = `sil;			12'd543: toneR = `sil;

				12'd544: toneR = `sil;			12'd545: toneR = `sil;
				12'd546: toneR = `sil;			12'd547: toneR = `sil;
				12'd548: toneR = `sil;			12'd549: toneR = `sil;
				12'd550: toneR = `sil;			12'd551: toneR = `sil;
				12'd552: toneR = `g;			12'd553: toneR = `g;
				12'd554: toneR = `g;			12'd555: toneR = `g;
				12'd556: toneR = `g;			12'd557: toneR = `g;
				12'd558: toneR = `g;			12'd559: toneR = `g;
				
				//--- Measure 2---
				
				12'd560: toneR = `hf;			12'd561: toneR = `hf;
				12'd562: toneR = `hf;			12'd563: toneR = `hf;
				12'd564: toneR = `hf;			12'd565: toneR = `hf;
				12'd566: toneR = `hf;			12'd567: toneR = `hf;
				12'd568: toneR = `he;			12'd569: toneR = `he;
				12'd570: toneR = `he;			12'd571: toneR = `he;
				12'd572: toneR = `he;			12'd573: toneR = `he;
				12'd574: toneR = `he;			12'd575: toneR = `he;
				
				12'd576: toneR = `he;			12'd577: toneR = `he;
				12'd578: toneR = `he;			12'd579: toneR = `he;
				12'd580: toneR = `he;			12'd581: toneR = `he;
				12'd582: toneR = `he;			12'd583: toneR = `he;
				12'd584: toneR = `he;			12'd585: toneR = `he;
				12'd586: toneR = `he;			12'd587: toneR = `he;
				12'd588: toneR = `he;			12'd589: toneR = `he;
				12'd590: toneR = `he;			12'd591: toneR = `he;
				
				12'd592: toneR = `hf;			12'd593: toneR = `hf;
				12'd594: toneR = `hf;			12'd595: toneR = `hf;
				12'd596: toneR = `hf;			12'd597: toneR = `hf;
				12'd598: toneR = `hf;			12'd599: toneR = `hf;
				12'd600: toneR = `he;			12'd601: toneR = `he;
				12'd602: toneR = `he;			12'd603: toneR = `he;
				12'd604: toneR = `he;			12'd605: toneR = `he;
				12'd606: toneR = `he;			12'd607: toneR = `he;
				
				12'd608: toneR = `he;			12'd609: toneR = `he;
				12'd610: toneR = `he;			12'd611: toneR = `he;
				12'd612: toneR = `he;			12'd613: toneR = `he;
				12'd614: toneR = `he;			12'd615: toneR = `he;
				12'd616: toneR = `hd;			12'd617: toneR = `hd;
				12'd618: toneR = `hd;			12'd619: toneR = `hd;
				12'd620: toneR = `hd;			12'd621: toneR = `hd;
				12'd622: toneR = `hd;			12'd623: toneR = `hd;
				
				12'd624: toneR = `hd;			12'd625: toneR = `hd;
				12'd626: toneR = `hd;			12'd627: toneR = `hd;
				12'd628: toneR = `hd;			12'd629: toneR = `hd;
				12'd630: toneR = `hd;			12'd631: toneR = `hd;
				12'd632: toneR = `sil;			12'd633: toneR = `sil;
				12'd634: toneR = `sil;			12'd635: toneR = `sil;
				12'd636: toneR = `sil;			12'd637: toneR = `sil;
				12'd638: toneR = `sil;			12'd639: toneR = `sil;
				
				//---Measure 3---
				12'd640: toneR = `hc;			12'd641: toneR = `hc;
				12'd642: toneR = `hc;			12'd643: toneR = `hc;
				12'd644: toneR = `hc;			12'd645: toneR = `hc;
				12'd646: toneR = `hc;			12'd647: toneR = `hc;
				12'd648: toneR = `hc;			12'd649: toneR = `hc;
				12'd650: toneR = `hc;			12'd651: toneR = `hc;
				12'd652: toneR = `hc;			12'd653: toneR = `hc;
				12'd654: toneR = `hc;			12'd655: toneR = `hc;
				
				12'd656: toneR = `hd;			12'd657: toneR = `hd;
				12'd658: toneR = `hd;			12'd659: toneR = `hd;
				12'd660: toneR = `hd;			12'd661: toneR = `hd;
				12'd662: toneR = `hd;			12'd663: toneR = `hd;
				12'd664: toneR = `he;			12'd665: toneR = `he;
				12'd666: toneR = `he;			12'd667: toneR = `he;
				12'd668: toneR = `he;			12'd669: toneR = `he;
				12'd670: toneR = `he;			12'd671: toneR = `he;
				
				12'd672: toneR = `he;			12'd673: toneR = `he;
				12'd674: toneR = `he;			12'd675: toneR = `he;
				12'd676: toneR = `he;			12'd677: toneR = `he;
				12'd678: toneR = `he;			12'd679: toneR = `he;
				12'd680: toneR = `hc;			12'd681: toneR = `hc;
				12'd682: toneR = `hc;			12'd683: toneR = `hc;
				12'd684: toneR = `hc;			12'd685: toneR = `hc;
				12'd686: toneR = `hc;			12'd687: toneR = `hc;
				
				12'd688: toneR = `hc;			12'd689: toneR = `hc;
				12'd690: toneR = `hc;			12'd691: toneR = `hc;
				12'd692: toneR = `hc;			12'd693: toneR = `hc;
				12'd694: toneR = `hc;			12'd695: toneR = `hc;
				12'd696: toneR = `hc;			12'd697: toneR = `hc;
				12'd698: toneR = `hc;			12'd699: toneR = `hc;
				12'd700: toneR = `hc;			12'd701: toneR = `hc;
				12'd702: toneR = `sil;			12'd703: toneR = `sil;
				
				//---Measure 4---
				
				12'd704: toneR = `a;			12'd705: toneR = `a;
				12'd706: toneR = `a;			12'd707: toneR = `a;
				12'd708: toneR = `a;			12'd709: toneR = `a;
				12'd710: toneR = `a;			12'd711: toneR = `a;
				12'd712: toneR = `a;			12'd713: toneR = `a;
				12'd714: toneR = `a;			12'd715: toneR = `a;
				12'd716: toneR = `a;			12'd717: toneR = `a;
				12'd718: toneR = `a;			12'd719: toneR = `a;
				
				12'd720: toneR = `hc;			12'd721: toneR = `hc;
				12'd722: toneR = `hc;			12'd723: toneR = `hc;
				12'd724: toneR = `hc;			12'd725: toneR = `hc;
				12'd726: toneR = `hc;			12'd727: toneR = `hc;
				12'd728: toneR = `hg;			12'd729: toneR = `hg;
				12'd730: toneR = `hg;			12'd731: toneR = `hg;
				12'd732: toneR = `hg;			12'd733: toneR = `hg;
				12'd734: toneR = `hg;			12'd735: toneR = `hg;
				
				12'd736: toneR = `hg;			12'd737: toneR = `hg;
				12'd738: toneR = `hg;			12'd739: toneR = `hg;
				12'd740: toneR = `hg;			12'd741: toneR = `hg;
				12'd742: toneR = `hg;			12'd743: toneR = `hg;
				12'd744: toneR = `hc;			12'd745: toneR = `hc;
				12'd746: toneR = `hc;			12'd747: toneR = `hc;
				12'd748: toneR = `hc;			12'd749: toneR = `hc;
				12'd750: toneR = `hc;			12'd751: toneR = `hc;
				
				12'd752: toneR = `he;			12'd753: toneR = `he;
				12'd754: toneR = `he;			12'd755: toneR = `he;
				12'd756: toneR = `he;			12'd757: toneR = `he;
				12'd758: toneR = `sil;			12'd759: toneR = `sil;
				12'd760: toneR = `he;			12'd761: toneR = `he;
				12'd762: toneR = `he;			12'd763: toneR = `he;
				12'd764: toneR = `he;			12'd765: toneR = `he;
				12'd766: toneR = `he;			12'd767: toneR = `he;
				
				//---Measure 5---   
				
				12'd768: toneR = `he;			12'd769: toneR = `he;
				12'd770: toneR = `he;			12'd771: toneR = `he;
				12'd772: toneR = `he;			12'd773: toneR = `he;
				12'd774: toneR = `he;			12'd775: toneR = `he;
				12'd776: toneR = `he;			12'd777: toneR = `he;
				12'd778: toneR = `he;			12'd779: toneR = `he;
				12'd780: toneR = `he;			12'd781: toneR = `he;
				12'd782: toneR = `he;			12'd783: toneR = `he;
				
				12'd784: toneR = `he;			12'd785: toneR = `he;
				12'd786: toneR = `he;			12'd787: toneR = `he;
				12'd788: toneR = `he;			12'd789: toneR = `he;
				12'd790: toneR = `he;			12'd791: toneR = `he;
				12'd792: toneR = `he;			12'd793: toneR = `he;
				12'd794: toneR = `he;			12'd795: toneR = `he;
				12'd796: toneR = `he;			12'd797: toneR = `he;
				12'd798: toneR = `he;			12'd799: toneR = `he;
				
				12'd800: toneR = `he;			12'd801: toneR = `he;
				12'd802: toneR = `he;			12'd803: toneR = `he;
				12'd804: toneR = `he;			12'd805: toneR = `he;
				12'd806: toneR = `sil;			12'd807: toneR = `sil;
				12'd808: toneR = `g;			12'd809: toneR = `g;
				12'd810: toneR = `g;			12'd811: toneR = `g;
				12'd812: toneR = `g;			12'd813: toneR = `g;
				12'd814: toneR = `g;			12'd815: toneR = `g;
				
				12'd816: toneR = `hf;			12'd817: toneR = `hf;
				12'd818: toneR = `hf;			12'd819: toneR = `hf;
				12'd820: toneR = `hf;			12'd821: toneR = `hf;
				12'd822: toneR = `hf;			12'd823: toneR = `hf;
				12'd824: toneR = `he;			12'd825: toneR = `he;
				12'd826: toneR = `he;			12'd827: toneR = `he;
				12'd828: toneR = `he;			12'd829: toneR = `he;
				12'd830: toneR = `he;			12'd831: toneR = `he;
				
				//---Measure 6---
				
				12'd832: toneR = `he;			12'd833: toneR = `he;
				12'd834: toneR = `he;			12'd835: toneR = `he;
				12'd836: toneR = `he;			12'd837: toneR = `he;
				12'd838: toneR = `he;			12'd839: toneR = `he;
				12'd840: toneR = `he;			12'd841: toneR = `he;
				12'd842: toneR = `he;			12'd843: toneR = `he;
				12'd844: toneR = `he;			12'd845: toneR = `he;
				12'd846: toneR = `he;			12'd847: toneR = `he;
				
				12'd848: toneR = `hf;			12'd849: toneR = `hf;
				12'd850: toneR = `hf;			12'd851: toneR = `hf;
				12'd852: toneR = `hf;			12'd853: toneR = `hf;
				12'd854: toneR = `hf;			12'd855: toneR = `hf;
				12'd856: toneR = `he;			12'd857: toneR = `he;
				12'd858: toneR = `he;			12'd859: toneR = `he;
				12'd860: toneR = `he;			12'd861: toneR = `he;
				12'd862: toneR = `he;			12'd863: toneR = `he;
				
				12'd864: toneR = `he;			12'd865: toneR = `he;
				12'd866: toneR = `he;			12'd867: toneR = `he;
				12'd868: toneR = `he;			12'd869: toneR = `he;
				12'd870: toneR = `he;			12'd871: toneR = `he;
				12'd872: toneR = `hd;			12'd873: toneR = `hd;
				12'd874: toneR = `hd;			12'd875: toneR = `hd;
				12'd876: toneR = `hd;			12'd877: toneR = `hd;
				12'd878: toneR = `hd;			12'd879: toneR = `hd;
				
				12'd880: toneR = `hd;			12'd881: toneR = `hd;
				12'd882: toneR = `hd;			12'd883: toneR = `hd;
				12'd884: toneR = `hd;			12'd885: toneR = `hd;
				12'd886: toneR = `hd;			12'd887: toneR = `hd;
				12'd888: toneR = `hd;			12'd889: toneR = `hd;
				12'd890: toneR = `hd;			12'd891: toneR = `hd;
				12'd892: toneR = `sil;			12'd893: toneR = `sil;
				12'd894: toneR = `sil;			12'd895: toneR = `sil;
				
				//---Measure 7---
				
				12'd896: toneR = `hc;			12'd897: toneR = `hc;
				12'd898: toneR = `hc;			12'd899: toneR = `hc;
				12'd900: toneR = `hc;			12'd901: toneR = `hc;
				12'd902: toneR = `hc;			12'd903: toneR = `hc;
				12'd904: toneR = `hc;			12'd905: toneR = `hc;
				12'd906: toneR = `hc;			12'd907: toneR = `hc;
				12'd908: toneR = `hc;			12'd909: toneR = `hc;
				12'd910: toneR = `hc;			12'd911: toneR = `hc;
				
				12'd912: toneR = `hd;			12'd913: toneR = `hd;
				12'd914: toneR = `hd;			12'd915: toneR = `hd;
				12'd916: toneR = `hd;			12'd917: toneR = `hd;
				12'd918: toneR = `hd;			12'd919: toneR = `hd;
				12'd920: toneR = `he;			12'd921: toneR = `he;
				12'd922: toneR = `he;			12'd923: toneR = `he;
				12'd924: toneR = `he;			12'd925: toneR = `he;
				12'd926: toneR = `he;			12'd927: toneR = `he;
				
				12'd928: toneR = `he;			12'd929: toneR = `he;
				12'd930: toneR = `he;			12'd931: toneR = `he;
				12'd932: toneR = `he;			12'd933: toneR = `he;
				12'd934: toneR = `he;			12'd935: toneR = `he;
				12'd936: toneR = `ha;			12'd937: toneR = `ha;
				12'd938: toneR = `ha;			12'd939: toneR = `ha;
				12'd940: toneR = `ha;			12'd941: toneR = `ha;
				12'd942: toneR = `ha;			12'd943: toneR = `ha;
				
				12'd944: toneR = `ha;			12'd945: toneR = `ha;
				12'd946: toneR = `ha;			12'd947: toneR = `ha;
				12'd948: toneR = `ha;			12'd949: toneR = `ha;
				12'd950: toneR = `ha;			12'd951: toneR = `ha;
				12'd952: toneR = `ha;			12'd953: toneR = `ha;
				12'd954: toneR = `ha;			12'd955: toneR = `ha;
				12'd956: toneR = `sil;			12'd957: toneR = `sil;
				12'd958: toneR = `sil;			12'd959: toneR = `sil;
				
				//---Measure 8---
				
				12'd960: toneR = `he;			12'd961: toneR = `he;
				12'd962: toneR = `he;			12'd963: toneR = `he;
				12'd964: toneR = `he;			12'd965: toneR = `he;
				12'd966: toneR = `he;			12'd967: toneR = `he;
				12'd968: toneR = `he;			12'd969: toneR = `he;
				12'd970: toneR = `he;			12'd971: toneR = `he;
				12'd972: toneR = `he;			12'd973: toneR = `he;
				12'd974: toneR = `he;			12'd975: toneR = `he;
				
				12'd976: toneR = `a;			12'd977: toneR = `a;
				12'd978: toneR = `a;			12'd979: toneR = `a;
				12'd980: toneR = `a;			12'd981: toneR = `a;
				12'd982: toneR = `a;			12'd983: toneR = `a;
				12'd984: toneR = `hc;			12'd985: toneR = `hc;
				12'd986: toneR = `hc;			12'd987: toneR = `hc;
				12'd988: toneR = `hc;			12'd989: toneR = `hc;
				12'd990: toneR = `hc;			12'd991: toneR = `hc;
				
				12'd992: toneR = `hc;			12'd993: toneR = `hc;
				12'd994: toneR = `hc;			12'd995: toneR = `hc;
				12'd996: toneR = `hc;			12'd997: toneR = `hc;
				12'd998: toneR = `hc;			12'd999: toneR = `hc;
				12'd1000: toneR = `hd;			12'd1001: toneR = `hd;
				12'd1002: toneR = `hd;			12'd1003: toneR = `hd;
				12'd1004: toneR = `hd;			12'd1005: toneR = `hd;
				12'd1006: toneR = `hd;			12'd1007: toneR = `hd;
				
				12'd1008: toneR = `hd;			12'd1009: toneR = `hd;
				12'd1010: toneR = `hd;			12'd1011: toneR = `hd;
				12'd1012: toneR = `sil;			12'd1013: toneR = `sil;
				12'd1014: toneR = `sil;			12'd1015: toneR = `sil;
				12'd1016: toneR = `hd;			12'd1017: toneR = `hd;
				12'd1018: toneR = `hd;			12'd1019: toneR = `hd;
				12'd1020: toneR = `hd;			12'd1021: toneR = `hd;
				12'd1022: toneR = `hd;			12'd1023: toneR = `hd;
				
				12'd1024: toneR = `hd;			12'd1025: toneR = `hd;
				12'd1026: toneR = `hd;			12'd1027: toneR = `hd;
				12'd1028: toneR = `hd;			12'd1029: toneR = `hd;
				12'd1030: toneR = `hd;			12'd1031: toneR = `hd;
				12'd1032: toneR = `hc;			12'd1033: toneR = `hc;
				12'd1034: toneR = `hc;			12'd1035: toneR = `hc;
				12'd1036: toneR = `hc;			12'd1037: toneR = `hc;
				12'd1038: toneR = `hc;			12'd1039: toneR = `hc;
				
				12'd1040: toneR = `hc;			12'd1041: toneR = `hc;
				12'd1042: toneR = `hc;			12'd1043: toneR = `hc;
				12'd1044: toneR = `hc;			12'd1045: toneR = `hc;
				12'd1046: toneR = `hc;			12'd1047: toneR = `hc;
				12'd1048: toneR = `hc;			12'd1049: toneR = `hc;
				12'd1050: toneR = `hc;			12'd1051: toneR = `hc;
				12'd1052: toneR = `hc;			12'd1053: toneR = `hc;
				12'd1054: toneR = `hc;			12'd1055: toneR = `hc;
                default: toneR = `sil;
            endcase
        end else begin
            toneR = `sil;
        end
    end

    always @(*) begin
        if(en==0)begin
            case(ibeatNum)
                // --- Measure 1 ---
                12'd0: toneL = `hc;  	12'd1: toneL = `hc; // HC (two-beat)
                12'd2: toneL = `hc;  	12'd3: toneL = `hc;
                12'd4: toneL = `hc;	    12'd5: toneL = `hc;
                12'd6: toneL = `hc;  	12'd7: toneL = `hc;
                12'd8: toneL = `hc;	    12'd9: toneL = `hc;
                12'd10: toneL = `hc;	12'd11: toneL = `hc;
                12'd12: toneL = `hc;	12'd13: toneL = `hc;
                12'd14: toneL = `hc;	12'd15: toneL = `hc;

                12'd16: toneL = `hc;	12'd17: toneL = `hc;
                12'd18: toneL = `hc;	12'd19: toneL = `hc;
                12'd20: toneL = `hc;	12'd21: toneL = `hc;
                12'd22: toneL = `hc;	12'd23: toneL = `hc;
                12'd24: toneL = `hc;	12'd25: toneL = `hc;
                12'd26: toneL = `hc;	12'd27: toneL = `hc;
                12'd28: toneL = `hc;	12'd29: toneL = `hc;
                12'd30: toneL = `hc;	12'd31: toneL = `hc;

                12'd32: toneL = `g;	    12'd33: toneL = `g; // G (one-beat)
                12'd34: toneL = `g;	    12'd35: toneL = `g;
                12'd36: toneL = `g;	    12'd37: toneL = `g;
                12'd38: toneL = `g;	    12'd39: toneL = `g;
                12'd40: toneL = `g;	    12'd41: toneL = `g;
                12'd42: toneL = `g;	    12'd43: toneL = `g;
                12'd44: toneL = `g;	    12'd45: toneL = `g;
                12'd46: toneL = `g;	    12'd47: toneL = `g;

                12'd48: toneL = `b;	    12'd49: toneL = `b; // B (one-beat)
                12'd50: toneL = `b;	    12'd51: toneL = `b;
                12'd52: toneL = `b;	    12'd53: toneL = `b;
                12'd54: toneL = `b;	    12'd55: toneL = `b;
                12'd56: toneL = `b;	    12'd57: toneL = `b;
                12'd58: toneL = `b;	    12'd59: toneL = `b;
                12'd60: toneL = `b;	    12'd61: toneL = `b;
                12'd62: toneL = `b;	    12'd63: toneL = `b;

                // --- Measure 2 ---
                
                12'd64: toneL = `hc;	    12'd65: toneL = `hc; // HC (two-beat)
                12'd66: toneL = `hc;	    12'd67: toneL = `hc;
                12'd68: toneL = `hc;	    12'd69: toneL = `hc;
                12'd70: toneL = `hc;	    12'd71: toneL = `hc;
                12'd72: toneL = `hc;	    12'd73: toneL = `hc;
                12'd74: toneL = `hc;	    12'd75: toneL = `hc;
                12'd76: toneL = `hc;	    12'd77: toneL = `hc;
                12'd78: toneL = `hc;	    12'd79: toneL = `hc;

                12'd80: toneL = `hc;	    12'd81: toneL = `hc;
                12'd82: toneL = `hc;	    12'd83: toneL = `hc;
                12'd84: toneL = `hc;	    12'd85: toneL = `hc;
                12'd86: toneL = `hc;	    12'd87: toneL = `hc;
                12'd88: toneL = `hc;	    12'd89: toneL = `hc;
                12'd90: toneL = `hc;	    12'd91: toneL = `hc;
                12'd92: toneL = `hc;	    12'd93: toneL = `hc;
                12'd94: toneL = `hc;	    12'd95: toneL = `hc;

                12'd96: toneL = `g;	    12'd97: toneL = `g; // G (one-beat)
                12'd98: toneL = `g; 	12'd99: toneL = `g;
                12'd100: toneL = `g;	12'd101: toneL = `g;
                12'd102: toneL = `g;	12'd103: toneL = `g;
                12'd104: toneL = `g;	12'd105: toneL = `g;
                12'd106: toneL = `g;	12'd107: toneL = `g;
                12'd108: toneL = `g;	12'd109: toneL = `g;
                12'd110: toneL = `g;	12'd111: toneL = `g;

                12'd112: toneL = `b;	12'd113: toneL = `b; // B (one-beat)
                12'd114: toneL = `b;	12'd115: toneL = `b;
                12'd116: toneL = `b;	12'd117: toneL = `b;
                12'd118: toneL = `b;	12'd119: toneL = `b;
                12'd120: toneL = `b;	12'd121: toneL = `b;
                12'd122: toneL = `b;	12'd123: toneL = `b;
                12'd124: toneL = `b;	12'd125: toneL = `b;
                12'd126: toneL = `b;	12'd127: toneL = `b;
                
                // --- Measure 3 ---
				12'd128: toneL = `hc;			12'd129: toneL = `hc;
				12'd130: toneL = `hc;			12'd131: toneL = `hc;
				12'd132: toneL = `hc;			12'd133: toneL = `hc;
				12'd134: toneL = `hc;			12'd135: toneL = `hc;
				12'd136: toneL = `hc;			12'd137: toneL = `hc;
				12'd138: toneL = `hc;			12'd139: toneL = `hc;
				12'd140: toneL = `hc;			12'd141: toneL = `hc;
				12'd142: toneL = `hc;			12'd143: toneL = `hc;

				12'd144: toneL = `hc;			12'd145: toneL = `hc;
				12'd146: toneL = `hc;			12'd147: toneL = `hc;
				12'd148: toneL = `hc;			12'd149: toneL = `hc;
				12'd150: toneL = `hc;			12'd151: toneL = `hc;
				12'd152: toneL = `hc;			12'd153: toneL = `hc;
				12'd154: toneL = `hc;			12'd155: toneL = `hc;
				12'd156: toneL = `hc;			12'd157: toneL = `hc;
				12'd158: toneL = `hc;			12'd159: toneL = `hc;

				12'd160: toneL = `g;			12'd161: toneL = `g;
				12'd162: toneL = `g;			12'd163: toneL = `g;
				12'd164: toneL = `g;			12'd165: toneL = `g;
				12'd166: toneL = `g;			12'd167: toneL = `g;
				12'd168: toneL = `g;			12'd169: toneL = `g;
				12'd170: toneL = `g;			12'd171: toneL = `g;
				12'd172: toneL = `g;			12'd173: toneL = `g;
				12'd174: toneL = `g;			12'd175: toneL = `g;

				12'd176: toneL = `b;			12'd177: toneL = `b;
				12'd178: toneL = `b;			12'd179: toneL = `b;
				12'd180: toneL = `b;			12'd181: toneL = `b;
				12'd182: toneL = `b;			12'd183: toneL = `b;
				12'd184: toneL = `b;			12'd185: toneL = `b;
				12'd186: toneL = `b;			12'd187: toneL = `b;
				12'd188: toneL = `b;			12'd189: toneL = `b;
				12'd190: toneL = `b;			12'd191: toneL = `b;

				// --- Measure 4 ---
				12'd192: toneL = `hc;			12'd193: toneL = `hc;
				12'd194: toneL = `hc;			12'd195: toneL = `hc;
				12'd196: toneL = `hc;			12'd197: toneL = `hc;
				12'd198: toneL = `hc;			12'd199: toneL = `hc;
				12'd200: toneL = `hc;			12'd201: toneL = `hc;
				12'd202: toneL = `hc;			12'd203: toneL = `hc;
				12'd204: toneL = `hc;			12'd205: toneL = `hc;
				12'd206: toneL = `hc;			12'd207: toneL = `hc;

				12'd208: toneL = `g;			12'd209: toneL = `g;
				12'd210: toneL = `g;			12'd211: toneL = `g;
				12'd212: toneL = `g;			12'd213: toneL = `g;
				12'd214: toneL = `g;			12'd215: toneL = `g;
				12'd216: toneL = `g;			12'd217: toneL = `g;
				12'd218: toneL = `g;			12'd219: toneL = `g;
				12'd220: toneL = `g;			12'd221: toneL = `g;
				12'd222: toneL = `g;			12'd223: toneL = `g;

				12'd224: toneL = `e;			12'd225: toneL = `e;
				12'd226: toneL = `e;			12'd227: toneL = `e;
				12'd228: toneL = `e;			12'd229: toneL = `e;
				12'd230: toneL = `e;			12'd231: toneL = `e;
				12'd232: toneL = `e;			12'd233: toneL = `e;
				12'd234: toneL = `e;			12'd235: toneL = `e;
				12'd236: toneL = `e;			12'd237: toneL = `e;
				12'd238: toneL = `e;			12'd239: toneL = `e;

				12'd240: toneL = `d;			12'd241: toneL = `d;
				12'd242: toneL = `d;			12'd243: toneL = `d;
				12'd244: toneL = `d;			12'd245: toneL = `d;
				12'd246: toneL = `d;			12'd247: toneL = `d;
				12'd248: toneL = `d;			12'd249: toneL = `d;
				12'd250: toneL = `d;			12'd251: toneL = `d;
				12'd252: toneL = `d;			12'd253: toneL = `d;
				12'd254: toneL = `d;			12'd255: toneL = `d;

				// --- Measure 5 ---
				12'd256: toneL = `g;			12'd257: toneL = `g;
				12'd258: toneL = `g;			12'd259: toneL = `g;
				12'd260: toneL = `g;			12'd261: toneL = `g;
				12'd262: toneL = `g;			12'd263: toneL = `g;
				12'd264: toneL = `g;			12'd265: toneL = `g;
				12'd266: toneL = `g;			12'd267: toneL = `g;
				12'd268: toneL = `g;			12'd269: toneL = `g;
				12'd270: toneL = `g;			12'd271: toneL = `g;

				12'd272: toneL = `g;			12'd273: toneL = `g;
				12'd274: toneL = `g;			12'd275: toneL = `g;
				12'd276: toneL = `g;			12'd277: toneL = `g;
				12'd278: toneL = `g;			12'd279: toneL = `g;
				12'd280: toneL = `g;			12'd281: toneL = `g;
				12'd282: toneL = `g;			12'd283: toneL = `g;
				12'd284: toneL = `g;			12'd285: toneL = `g;
				12'd286: toneL = `g;			12'd287: toneL = `g;

				12'd288: toneL = `f;			12'd289: toneL = `f;
				12'd290: toneL = `f;			12'd291: toneL = `f;
				12'd292: toneL = `f;			12'd293: toneL = `f;
				12'd294: toneL = `f;			12'd295: toneL = `f;
				12'd296: toneL = `f;			12'd297: toneL = `f;
				12'd298: toneL = `f;			12'd299: toneL = `f;
				12'd300: toneL = `f;			12'd301: toneL = `f;
				12'd302: toneL = `f;			12'd303: toneL = `f;

				12'd304: toneL = `d;			12'd305: toneL = `d;
				12'd306: toneL = `d;			12'd307: toneL = `d;
				12'd308: toneL = `d;			12'd309: toneL = `d;
				12'd310: toneL = `d;			12'd311: toneL = `d;
				12'd312: toneL = `d;			12'd313: toneL = `d;
				12'd314: toneL = `d;			12'd315: toneL = `d;
				12'd316: toneL = `d;			12'd317: toneL = `d;
				12'd318: toneL = `d;			12'd319: toneL = `d;

				// --- Measure 6 ---
				12'd320: toneL = `e;			12'd321: toneL = `e;
				12'd322: toneL = `e;			12'd323: toneL = `e;
				12'd324: toneL = `e;			12'd325: toneL = `e;
				12'd326: toneL = `e;			12'd327: toneL = `e;
				12'd328: toneL = `e;			12'd329: toneL = `e;
				12'd330: toneL = `e;			12'd331: toneL = `e;
				12'd332: toneL = `e;			12'd333: toneL = `e;
				12'd334: toneL = `e;			12'd335: toneL = `e;

				12'd336: toneL = `e;			12'd337: toneL = `e;
				12'd338: toneL = `e;			12'd339: toneL = `e;
				12'd340: toneL = `e;			12'd341: toneL = `e;
				12'd342: toneL = `e;			12'd343: toneL = `e;
				12'd344: toneL = `e;			12'd345: toneL = `e;
				12'd346: toneL = `e;			12'd347: toneL = `e;
				12'd348: toneL = `e;			12'd349: toneL = `e;
				12'd350: toneL = `e;			12'd351: toneL = `e;

				12'd352: toneL = `g;			12'd353: toneL = `g;
				12'd354: toneL = `g;			12'd355: toneL = `g;
				12'd356: toneL = `g;			12'd357: toneL = `g;
				12'd358: toneL = `g;			12'd359: toneL = `g;
				12'd360: toneL = `g;			12'd361: toneL = `g;
				12'd362: toneL = `g;			12'd363: toneL = `g;
				12'd364: toneL = `g;			12'd365: toneL = `g;
				12'd366: toneL = `g;			12'd367: toneL = `g;

				12'd368: toneL = `b;			12'd369: toneL = `b;
				12'd370: toneL = `b;			12'd371: toneL = `b;
				12'd372: toneL = `b;			12'd373: toneL = `b;
				12'd374: toneL = `b;			12'd375: toneL = `b;
				12'd376: toneL = `b;			12'd377: toneL = `b;
				12'd378: toneL = `b;			12'd379: toneL = `b;
				12'd380: toneL = `b;			12'd381: toneL = `b;
				12'd382: toneL = `b;			12'd383: toneL = `b;

				// --- Measure 7 ---
				12'd384: toneL = `hc;			12'd385: toneL = `hc;
				12'd386: toneL = `hc;			12'd387: toneL = `hc;
				12'd388: toneL = `hc;			12'd389: toneL = `hc;
				12'd390: toneL = `hc;			12'd391: toneL = `hc;
				12'd392: toneL = `hc;			12'd393: toneL = `hc;
				12'd394: toneL = `hc;			12'd395: toneL = `hc;
				12'd396: toneL = `hc;			12'd397: toneL = `hc;
				12'd398: toneL = `hc;			12'd399: toneL = `hc;

				12'd400: toneL = `hc;			12'd401: toneL = `hc;
				12'd402: toneL = `hc;			12'd403: toneL = `hc;
				12'd404: toneL = `hc;			12'd405: toneL = `hc;
				12'd406: toneL = `hc;			12'd407: toneL = `hc;
				12'd408: toneL = `hc;			12'd409: toneL = `hc;
				12'd410: toneL = `hc;			12'd411: toneL = `hc;
				12'd412: toneL = `hc;			12'd413: toneL = `hc;
				12'd414: toneL = `hc;			12'd415: toneL = `hc;

				12'd416: toneL = `g;			12'd417: toneL = `g;
				12'd418: toneL = `g;			12'd419: toneL = `g;
				12'd420: toneL = `g;			12'd421: toneL = `g;
				12'd422: toneL = `g;			12'd423: toneL = `g;
				12'd424: toneL = `g;			12'd425: toneL = `g;
				12'd426: toneL = `g;			12'd427: toneL = `g;
				12'd428: toneL = `g;			12'd429: toneL = `g;
				12'd430: toneL = `g;			12'd431: toneL = `g;

				12'd432: toneL = `b;			12'd433: toneL = `b;
				12'd434: toneL = `b;			12'd435: toneL = `b;
				12'd436: toneL = `b;			12'd437: toneL = `b;
				12'd438: toneL = `b;			12'd439: toneL = `b;
				12'd440: toneL = `b;			12'd441: toneL = `b;
				12'd442: toneL = `b;			12'd443: toneL = `b;
				12'd444: toneL = `b;			12'd445: toneL = `b;
				12'd446: toneL = `b;			12'd447: toneL = `b;

				// --- Measure 8 ---
				12'd448: toneL = `c;			12'd449: toneL = `c;
				12'd450: toneL = `c;			12'd451: toneL = `c;
				12'd452: toneL = `c;			12'd453: toneL = `c;
				12'd454: toneL = `c;			12'd455: toneL = `c;
                12'd456: toneL = `c;			12'd457: toneL = `c;
				12'd458: toneL = `c;			12'd459: toneL = `c;
				12'd460: toneL = `c;			12'd461: toneL = `c;
				12'd462: toneL = `c;			12'd463: toneL = `c;

				12'd464: toneL = `g;			12'd465: toneL = `g;
				12'd466: toneL = `g;			12'd467: toneL = `g;
				12'd468: toneL = `g;			12'd469: toneL = `g;
				12'd470: toneL = `g;			12'd471: toneL = `g;
				12'd472: toneL = `g;			12'd473: toneL = `g;
				12'd474: toneL = `g;			12'd475: toneL = `g;
				12'd476: toneL = `g;			12'd477: toneL = `g;
				12'd478: toneL = `g;			12'd479: toneL = `g;

				12'd480: toneL = `c;			12'd481: toneL = `c;
				12'd482: toneL = `c;			12'd483: toneL = `c;
				12'd484: toneL = `c;			12'd485: toneL = `c;
				12'd486: toneL = `c;			12'd487: toneL = `c;
				12'd488: toneL = `c;			12'd489: toneL = `c;
				12'd490: toneL = `c;			12'd491: toneL = `c;
				12'd492: toneL = `c;			12'd493: toneL = `c;
				12'd494: toneL = `c;			12'd495: toneL = `c;

				12'd496: toneL = `d;			12'd497: toneL = `d;
				12'd498: toneL = `d;			12'd499: toneL = `d;
				12'd500: toneL = `d;			12'd501: toneL = `d;
				12'd502: toneL = `d;			12'd503: toneL = `d;
				12'd504: toneL = `d;			12'd505: toneL = `d;
				12'd506: toneL = `d;			12'd507: toneL = `d;
				12'd508: toneL = `d;			12'd509: toneL = `d;
				12'd510: toneL = `d;			12'd511: toneL = `d;     
                
                // gaobaiqiqiu
                
                12'd512: toneL = `sil;			12'd513: toneL = `sil;
				12'd514: toneL = `sil;			12'd515: toneL = `sil;
				12'd516: toneL = `sil;			12'd517: toneL = `sil;
				12'd518: toneL = `sil;			12'd519: toneL = `sil;
				12'd520: toneL = `sil;			12'd521: toneL = `sil;
				12'd522: toneL = `sil;			12'd523: toneL = `sil;
				12'd524: toneL = `sil;			12'd525: toneL = `sil;
				12'd526: toneL = `sil;			12'd527: toneL = `sil;
				
				12'd528: toneL = `sil;			12'd529: toneL = `sil;
				12'd530: toneL = `sil;			12'd531: toneL = `sil;
				12'd532: toneL = `sil;			12'd533: toneL = `sil;
				12'd534: toneL = `sil;			12'd535: toneL = `sil;
				12'd536: toneL = `sil;			12'd537: toneL = `sil;
				12'd538: toneL = `sil;			12'd539: toneL = `sil;
				12'd540: toneL = `sil;			12'd541: toneL = `sil;
				12'd542: toneL = `sil;			12'd543: toneL = `sil;
				
				12'd544: toneL = `sil;			12'd545: toneL = `sil;
				12'd546: toneL = `sil;			12'd547: toneL = `sil;
				12'd548: toneL = `sil;			12'd549: toneL = `sil;
				12'd550: toneL = `sil;			12'd551: toneL = `sil;
				12'd552: toneL = `sil;			12'd553: toneL = `sil;
				12'd554: toneL = `sil;			12'd555: toneL = `sil;
				12'd556: toneL = `sil;			12'd557: toneL = `sil;
				12'd558: toneL = `sil;			12'd559: toneL = `sil;
				
				12'd560: toneL = `sil;			12'd561: toneL = `sil;
				12'd562: toneL = `sil;			12'd563: toneL = `sil;
				12'd564: toneL = `sil;			12'd565: toneL = `sil;
				12'd566: toneL = `sil;			12'd567: toneL = `sil;
				12'd568: toneL = `sil;			12'd569: toneL = `sil;
				12'd570: toneL = `sil;			12'd571: toneL = `sil;
				12'd572: toneL = `sil;			12'd573: toneL = `sil;
				12'd574: toneL = `sil;			12'd575: toneL = `sil;
				
				//---Measure 2
				12'd576: toneL = `c;			12'd577: toneL = `c;
				12'd578: toneL = `c;			12'd579: toneL = `c;
				12'd580: toneL = `c;			12'd581: toneL = `c;
				12'd582: toneL = `c;			12'd583: toneL = `c;
				12'd584: toneL = `e;			12'd585: toneL = `e;
				12'd586: toneL = `e;			12'd587: toneL = `e;
				12'd588: toneL = `e;			12'd589: toneL = `e;
				12'd590: toneL = `e;			12'd591: toneL = `e;
				
				12'd592: toneL = `g;			12'd593: toneL = `g;
				12'd594: toneL = `g;			12'd595: toneL = `g;
				12'd596: toneL = `g;			12'd597: toneL = `g;
				12'd598: toneL = `g;			12'd599: toneL = `g;
				12'd600: toneL = `g;			12'd601: toneL = `g;
				12'd602: toneL = `g;			12'd603: toneL = `g;
				12'd604: toneL = `g;			12'd605: toneL = `g;
				12'd606: toneL = `g;			12'd607: toneL = `g;
				
				12'd608: toneL = `e;			12'd609: toneL = `e;
				12'd610: toneL = `e;			12'd611: toneL = `e;
				12'd612: toneL = `e;			12'd613: toneL = `e;
				12'd614: toneL = `e;			12'd615: toneL = `e;
				12'd616: toneL = `gs;			12'd617: toneL = `gs;
				12'd618: toneL = `gs;			12'd619: toneL = `gs;
				12'd620: toneL = `gs;			12'd621: toneL = `gs;
				12'd622: toneL = `gs;			12'd623: toneL = `gs;
				
				12'd624: toneL = `b;			12'd625: toneL = `b;
				12'd626: toneL = `b;			12'd627: toneL = `b;
				12'd628: toneL = `b;			12'd629: toneL = `b;
				12'd630: toneL = `b;			12'd631: toneL = `b;
				12'd632: toneL = `a;			12'd633: toneL = `a;
				12'd634: toneL = `a;			12'd635: toneL = `a;
				12'd636: toneL = `a;			12'd637: toneL = `a;
				12'd638: toneL = `a;			12'd639: toneL = `a;
				
				//---Measure 3---
				12'd640: toneL = `a;			12'd641: toneL = `a;
				12'd642: toneL = `a;			12'd643: toneL = `a;
				12'd644: toneL = `a;			12'd645: toneL = `a;
				12'd646: toneL = `a;			12'd647: toneL = `a;
				12'd648: toneL = `e;			12'd649: toneL = `e;
				12'd650: toneL = `e;			12'd651: toneL = `e;
				12'd652: toneL = `e;			12'd653: toneL = `e;
				12'd654: toneL = `e;			12'd655: toneL = `e;
				
				12'd656: toneL = `a;			12'd657: toneL = `a;
				12'd658: toneL = `a;			12'd659: toneL = `a;
				12'd660: toneL = `a;			12'd661: toneL = `a;
				12'd662: toneL = `a;			12'd663: toneL = `a;
				12'd664: toneL = `a;			12'd665: toneL = `a;
				12'd666: toneL = `a;			12'd667: toneL = `a;
				12'd668: toneL = `a;			12'd669: toneL = `a;
				12'd670: toneL = `a;			12'd671: toneL = `a;
				
				12'd672: toneL = `a;			12'd673: toneL = `a;
				12'd674: toneL = `a;			12'd675: toneL = `a;
				12'd676: toneL = `sil;			12'd677: toneL = `sil;
				12'd678: toneL = `sil;			12'd679: toneL = `sil;
				12'd680: toneL = `hc;			12'd681: toneL = `hc;
				12'd682: toneL = `hc;			12'd683: toneL = `hc;
				12'd684: toneL = `hc;			12'd685: toneL = `hc;
				12'd686: toneL = `hc;			12'd687: toneL = `hc;
				
				12'd688: toneL = `g;			12'd689: toneL = `g;
				12'd690: toneL = `g;			12'd691: toneL = `g;
				12'd692: toneL = `g;			12'd693: toneL = `g;
				12'd694: toneL = `g;			12'd695: toneL = `g;
				12'd696: toneL = `e;			12'd697: toneL = `e;
				12'd698: toneL = `e;			12'd699: toneL = `e;
				12'd700: toneL = `e;			12'd701: toneL = `e;
				12'd702: toneL = `e;			12'd703: toneL = `e;
				
				//---Measure 4---
				12'd704: toneL = `f;			12'd705: toneL = `f;
				12'd706: toneL = `f;			12'd707: toneL = `f;
				12'd708: toneL = `f;			12'd709: toneL = `f;
				12'd710: toneL = `f;			12'd711: toneL = `f;
				12'd712: toneL = `f;			12'd713: toneL = `f;
				12'd714: toneL = `f;			12'd715: toneL = `f;
				12'd716: toneL = `f;			12'd717: toneL = `f;
				12'd718: toneL = `f;			12'd719: toneL = `f;
				
				12'd720: toneL = `a;			12'd721: toneL = `a;
				12'd722: toneL = `a;			12'd723: toneL = `a;
				12'd724: toneL = `a;			12'd725: toneL = `a;
				12'd726: toneL = `a;			12'd727: toneL = `a;
				12'd728: toneL = `hc;			12'd729: toneL = `hc;
				12'd730: toneL = `hc;			12'd731: toneL = `hc;
				12'd732: toneL = `hc;			12'd733: toneL = `hc;
				12'd734: toneL = `hc;			12'd735: toneL = `hc;
				
				12'd736: toneL = `g;			12'd737: toneL = `g;
				12'd738: toneL = `g;			12'd739: toneL = `g;
				12'd740: toneL = `g;			12'd741: toneL = `g;
				12'd742: toneL = `sil;			12'd743: toneL = `sil;
				12'd744: toneL = `g;			12'd745: toneL = `g;
				12'd746: toneL = `g;			12'd747: toneL = `g;
				12'd748: toneL = `g;			12'd749: toneL = `g;
				12'd750: toneL = `g;			12'd751: toneL = `g;
				
				12'd752: toneL = `g;			12'd753: toneL = `g;
				12'd754: toneL = `g;			12'd755: toneL = `g;
				12'd756: toneL = `g;			12'd757: toneL = `g;
				12'd758: toneL = `sil;			12'd759: toneL = `sil;
				12'd760: toneL = `g;			12'd761: toneL = `g;
				12'd762: toneL = `g;			12'd763: toneL = `g;
				12'd764: toneL = `g;			12'd765: toneL = `g;
				12'd766: toneL = `g;			12'd767: toneL = `g;
				
				//---Measure 5---
				12'd768: toneL = `g;			12'd769: toneL = `g;
				12'd770: toneL = `g;			12'd771: toneL = `g;
				12'd772: toneL = `g;			12'd773: toneL = `g;
				12'd774: toneL = `sil;			12'd775: toneL = `sil;
				12'd776: toneL = `g;			12'd777: toneL = `g;
				12'd778: toneL = `g;			12'd779: toneL = `g;
				12'd780: toneL = `g;			12'd781: toneL = `g;
				12'd782: toneL = `g;			12'd783: toneL = `g;
				
				12'd784: toneL = `e;			12'd785: toneL = `e;
				12'd786: toneL = `e;			12'd787: toneL = `e;
				12'd788: toneL = `e;			12'd789: toneL = `e;
				12'd790: toneL = `e;			12'd791: toneL = `e;
				12'd792: toneL = `g;			12'd793: toneL = `g;
				12'd794: toneL = `g;			12'd795: toneL = `g;
				12'd796: toneL = `g;			12'd797: toneL = `g;
				12'd798: toneL = `g;			12'd799: toneL = `g;
				
				12'd800: toneL = `b;			12'd801: toneL = `b;
				12'd802: toneL = `b;			12'd803: toneL = `b;
				12'd804: toneL = `b;			12'd805: toneL = `b;
				12'd806: toneL = `b;			12'd807: toneL = `b;
				12'd808: toneL = `b;			12'd809: toneL = `b;
				12'd810: toneL = `b;			12'd811: toneL = `b;
				12'd812: toneL = `b;			12'd813: toneL = `b;
				12'd814: toneL = `b;			12'd815: toneL = `b;
				
				12'd816: toneL = `b;			12'd817: toneL = `b;
				12'd818: toneL = `b;			12'd819: toneL = `b;
				12'd820: toneL = `b;			12'd821: toneL = `b;
				12'd822: toneL = `b;			12'd823: toneL = `b;
				12'd824: toneL = `b;			12'd825: toneL = `b;
				12'd826: toneL = `b;			12'd827: toneL = `b;
				12'd828: toneL = `b;			12'd829: toneL = `b;
				12'd830: toneL = `sil;			12'd831: toneL = `sil;
				
				//---Measure 6---
				12'd832: toneL = `c;			12'd833: toneL = `c;
				12'd834: toneL = `c;			12'd835: toneL = `c;
				12'd836: toneL = `c;			12'd837: toneL = `c;
				12'd838: toneL = `c;			12'd839: toneL = `c;
                12'd840: toneL = `e;			12'd841: toneL = `e;
				12'd842: toneL = `e;			12'd843: toneL = `e;
				12'd844: toneL = `e;			12'd845: toneL = `e;
				12'd846: toneL = `e;			12'd847: toneL = `e;
				
				12'd848: toneL = `g;			12'd849: toneL = `g;
				12'd850: toneL = `g;			12'd851: toneL = `g;
				12'd852: toneL = `g;			12'd853: toneL = `g;
				12'd854: toneL = `g;			12'd855: toneL = `g;
				12'd856: toneL = `g;			12'd857: toneL = `g;
				12'd858: toneL = `g;			12'd859: toneL = `g;
				12'd860: toneL = `g;			12'd861: toneL = `g;
				12'd862: toneL = `g;			12'd863: toneL = `g;
				
				12'd864: toneL = `e;			12'd865: toneL = `e;
				12'd866: toneL = `e;			12'd867: toneL = `e;
				12'd868: toneL = `e;			12'd869: toneL = `e;
				12'd870: toneL = `e;			12'd871: toneL = `e;
				12'd872: toneL = `gs;			12'd873: toneL = `gs;
				12'd874: toneL = `gs;			12'd875: toneL = `gs;
				12'd876: toneL = `gs;			12'd877: toneL = `gs;
				12'd878: toneL = `gs;			12'd879: toneL = `gs;
				
				12'd880: toneL = `b;			12'd881: toneL = `b;
				12'd882: toneL = `b;			12'd883: toneL = `b;
				12'd884: toneL = `b;			12'd885: toneL = `b;
				12'd886: toneL = `b;			12'd887: toneL = `b;
				12'd888: toneL = `a;			12'd889: toneL = `a;
				12'd890: toneL = `a;			12'd891: toneL = `a;
				12'd892: toneL = `a;			12'd893: toneL = `a;
				12'd894: toneL = `a;			12'd895: toneL = `a;
				
				//---Measure 7---
				12'd896: toneL = `a;			12'd897: toneL = `a;
				12'd898: toneL = `a;			12'd899: toneL = `a;
				12'd900: toneL = `a;			12'd901: toneL = `a;
				12'd902: toneL = `a;			12'd903: toneL = `a;
				12'd904: toneL = `e;			12'd905: toneL = `e;
				12'd906: toneL = `e;			12'd907: toneL = `e;
				12'd908: toneL = `e;			12'd909: toneL = `e;
				12'd910: toneL = `e;			12'd911: toneL = `e;
				
				12'd912: toneL = `a;			12'd913: toneL = `a;
				12'd914: toneL = `a;			12'd915: toneL = `a;
				12'd916: toneL = `a;			12'd917: toneL = `a;
				12'd918: toneL = `a;			12'd919: toneL = `a;
				12'd920: toneL = `a;			12'd921: toneL = `a;
				12'd922: toneL = `a;			12'd923: toneL = `a;
				12'd924: toneL = `a;			12'd925: toneL = `a;
				12'd926: toneL = `a;			12'd927: toneL = `a;
				
				12'd928: toneL = `a;			12'd929: toneL = `a;
				12'd930: toneL = `a;			12'd931: toneL = `a;
				12'd932: toneL = `a;			12'd933: toneL = `a;
				12'd934: toneL = `sil;			12'd935: toneL = `sil;
				12'd936: toneL = `he;			12'd937: toneL = `he;
				12'd938: toneL = `he;			12'd939: toneL = `he;
				12'd940: toneL = `he;			12'd941: toneL = `he;
				12'd942: toneL = `he;			12'd943: toneL = `he;
				
				12'd944: toneL = `hc;			12'd945: toneL = `hc;
				12'd946: toneL = `hc;			12'd947: toneL = `hc;
				12'd948: toneL = `hc;			12'd949: toneL = `hc;
				12'd950: toneL = `hc;			12'd951: toneL = `hc;
				12'd952: toneL = `a;			12'd953: toneL = `a;
				12'd954: toneL = `a;			12'd955: toneL = `a;
				12'd956: toneL = `a;			12'd957: toneL = `a;
				12'd958: toneL = `a;			12'd959: toneL = `a;
				
				//---Measure 8---
				12'd960: toneL = `c;			12'd961: toneL = `c;
				12'd962: toneL = `c;			12'd963: toneL = `c;
				12'd964: toneL = `c;			12'd965: toneL = `c;
				12'd966: toneL = `c;			12'd967: toneL = `c;
				12'd968: toneL = `e;			12'd969: toneL = `e;
				12'd970: toneL = `e;			12'd971: toneL = `e;
				12'd972: toneL = `e;			12'd973: toneL = `e;
				12'd974: toneL = `e;			12'd975: toneL = `e;
				
				12'd976: toneL = `a;			12'd977: toneL = `a;
				12'd978: toneL = `a;			12'd979: toneL = `a;
				12'd980: toneL = `a;			12'd981: toneL = `a;
				12'd982: toneL = `a;			12'd983: toneL = `a;
				12'd984: toneL = `hc;			12'd985: toneL = `hc;
				12'd986: toneL = `hc;			12'd987: toneL = `hc;
				12'd988: toneL = `hc;			12'd989: toneL = `hc;
				12'd990: toneL = `hc;			12'd991: toneL = `hc;
				
				12'd992: toneL = `hd;			12'd993: toneL = `hd;
				12'd994: toneL = `hd;			12'd995: toneL = `hd;
				12'd996: toneL = `hd;			12'd997: toneL = `hd;
				12'd998: toneL = `hd;			12'd999: toneL = `hd;
				12'd1000: toneL = `g;			12'd1001: toneL = `g;
				12'd1002: toneL = `g;			12'd1003: toneL = `g;
				12'd1004: toneL = `g;			12'd1005: toneL = `g;
				12'd1006: toneL = `g;			12'd1007: toneL = `g;
				
				12'd1008: toneL = `b;			12'd1009: toneL = `b;
				12'd1010: toneL = `b;			12'd1011: toneL = `b;
				12'd1012: toneL = `b;			12'd1013: toneL = `b;
				12'd1014: toneL = `b;			12'd1015: toneL = `b;
				12'd1016: toneL = `sil;			12'd1017: toneL = `sil;
				12'd1018: toneL = `sil;			12'd1019: toneL = `sil;
				12'd1020: toneL = `sil;			12'd1021: toneL = `sil;
				12'd1022: toneL = `sil;			12'd1023: toneL = `sil;
				
				//---Measure 9---
				12'd1024: toneL = `hc;			12'd1025: toneL = `hc;
				12'd1026: toneL = `hc;			12'd1027: toneL = `hc;
				12'd1028: toneL = `hc;			12'd1029: toneL = `hc;
				12'd1030: toneL = `hc;			12'd1031: toneL = `hc;
				12'd1032: toneL = `hc;			12'd1033: toneL = `hc;
				12'd1034: toneL = `hc;			12'd1035: toneL = `hc;
				12'd1036: toneL = `hc;			12'd1037: toneL = `hc;
				12'd1038: toneL = `hc;			12'd1039: toneL = `hc;
				
				12'd1040: toneL = `hc;			12'd1041: toneL = `hc;
				12'd1042: toneL = `hc;			12'd1043: toneL = `hc;
				12'd1044: toneL = `hc;			12'd1045: toneL = `hc;
				12'd1046: toneL = `hc;			12'd1047: toneL = `hc;
				12'd1048: toneL = `hc;			12'd1049: toneL = `hc;
				12'd1050: toneL = `hc;			12'd1051: toneL = `hc;
				12'd1052: toneL = `hc;			12'd1053: toneL = `hc;
				12'd1054: toneL = `hc;			12'd1055: toneL = `hc;
				
                default : toneL = `sil;
            endcase
        end
        else begin
            toneL = `sil;
        end
    end
endmodule

module speaker_control(
    clk,  // clock from the crystal
    rst,  // active high reset
    audio_in_left, // left channel audio data input
    audio_in_right, // right channel audio data input
    audio_mclk, // master clock
    audio_lrck, // left-right clock, Word Select clock, or sample rate clock
    audio_sck, // serial clock
    audio_sdin // serial audio data input
);

    // I/O declaration
    input clk;  // clock from the crystal
    input rst;  // active high reset
    input [15:0] audio_in_left; // left channel audio data input
    input [15:0] audio_in_right; // right channel audio data input
    output audio_mclk; // master clock
    output audio_lrck; // left-right clock
    output audio_sck; // serial clock
    output audio_sdin; // serial audio data input
    reg audio_sdin;

    // Declare internal signal nodes 
    wire [8:0] clk_cnt_next;
    reg [8:0] clk_cnt;
    reg [15:0] audio_left, audio_right;

    // Counter for the clock divider
    assign clk_cnt_next = clk_cnt + 1'b1;

    always @(posedge clk or posedge rst)
        if (rst == 1'b1)
            clk_cnt <= 9'd0;
        else
            clk_cnt <= clk_cnt_next;

    // Assign divided clock output
    assign audio_mclk = clk_cnt[1];
    assign audio_lrck = clk_cnt[8];
    assign audio_sck = 1'b1; // use internal serial clock mode

    // audio input data buffer
    always @(posedge clk_cnt[8] or posedge rst)
        if (rst == 1'b1)
            begin
                audio_left <= 16'd0;
                audio_right <= 16'd0;
            end
        else
            begin
                audio_left <= audio_in_left;
                audio_right <= audio_in_right;
            end

    always @*
        case (clk_cnt[8:4])
            5'b00000: audio_sdin = audio_right[0];
            5'b00001: audio_sdin = audio_left[15];
            5'b00010: audio_sdin = audio_left[14];
            5'b00011: audio_sdin = audio_left[13];
            5'b00100: audio_sdin = audio_left[12];
            5'b00101: audio_sdin = audio_left[11];
            5'b00110: audio_sdin = audio_left[10];
            5'b00111: audio_sdin = audio_left[9];
            5'b01000: audio_sdin = audio_left[8];
            5'b01001: audio_sdin = audio_left[7];
            5'b01010: audio_sdin = audio_left[6];
            5'b01011: audio_sdin = audio_left[5];
            5'b01100: audio_sdin = audio_left[4];
            5'b01101: audio_sdin = audio_left[3];
            5'b01110: audio_sdin = audio_left[2];
            5'b01111: audio_sdin = audio_left[1];
            5'b10000: audio_sdin = audio_left[0];
            5'b10001: audio_sdin = audio_right[15];
            5'b10010: audio_sdin = audio_right[14];
            5'b10011: audio_sdin = audio_right[13];
            5'b10100: audio_sdin = audio_right[12];
            5'b10101: audio_sdin = audio_right[11];
            5'b10110: audio_sdin = audio_right[10];
            5'b10111: audio_sdin = audio_right[9];
            5'b11000: audio_sdin = audio_right[8];
            5'b11001: audio_sdin = audio_right[7];
            5'b11010: audio_sdin = audio_right[6];
            5'b11011: audio_sdin = audio_right[5];
            5'b11100: audio_sdin = audio_right[4];
            5'b11101: audio_sdin = audio_right[3];
            5'b11110: audio_sdin = audio_right[2];
            5'b11111: audio_sdin = audio_right[1];
            default: audio_sdin = 1'b0;
        endcase

endmodule

module note_gen(
    clk, // clock from crystal
    rst, // active high reset
    note_div_left, // div for note generation
    note_div_right,
    audio_left,
    audio_right,
    volume
);

    // I/O declaration
    input clk; // clock from crystal
    input rst; // active low reset
    input [21:0] note_div_left, note_div_right; // div for note generation
    output reg [15:0] audio_left, audio_right;
    input [2:0] volume;

    // Declare internal signals
    reg [21:0] clk_cnt_next, clk_cnt;
    reg [21:0] clk_cnt_next_2, clk_cnt_2;
    reg b_clk, b_clk_next;
    reg c_clk, c_clk_next;

    // Note frequency generation
    always @(posedge clk or posedge rst)
        if (rst == 1'b1)
            begin
                clk_cnt <= 22'd0;
                clk_cnt_2 <= 22'd0;
                b_clk <= 1'b0;
                c_clk <= 1'b0;
            end
        else
            begin
                clk_cnt <= clk_cnt_next;
                clk_cnt_2 <= clk_cnt_next_2;
                b_clk <= b_clk_next;
                c_clk <= c_clk_next;
            end
        
    always @*
        if (clk_cnt == note_div_left)
            begin
                clk_cnt_next = 22'd0;
                b_clk_next = ~b_clk;
            end
        else
            begin
                clk_cnt_next = clk_cnt + 1'b1;
                b_clk_next = b_clk;
            end

    always @*
        if (clk_cnt_2 == note_div_right)
            begin
                clk_cnt_next_2 = 22'd0;
                c_clk_next = ~c_clk;
            end
        else
            begin
                clk_cnt_next_2 = clk_cnt_2 + 1'b1;
                c_clk_next = c_clk;
            end

    // Assign the amplitude of the note
    // Volume is controlled here
    //assign audio_left = (note_div_left == 22'd1) ? 16'h0000 : (b_clk == 1'b0) ? 16'hE000 : 16'h2000;
    //assign audio_right = (note_div_right == 22'd1) ? 16'h0000 : (c_clk == 1'b0) ? 16'hE000 : 16'h2000;
    
    always @(posedge clk) begin
        if(note_div_left == 22'd1000 && note_div_right == 22'd1000) begin
           audio_left = 16'h0;
           audio_right = 16'h0; 
        end 
        else if (note_div_left == 22'd1000) begin
            audio_left = 16'h0;
        end
        else if (note_div_right == 22'd1000) begin
            audio_right = 16'h0;
        end
        else begin
            if(volume == 0) begin
                audio_right = (b_clk) ? 16'h0FFF : 16'hF001;
                audio_left = (c_clk) ? 16'h0FFF : 16'hF001;
            end
            else if(volume == 1) begin
                audio_right = (b_clk) ? 16'h1FFF : 16'hE001;
                audio_left = (c_clk) ? 16'h1FFF : 16'hE001;
            end
            else if(volume == 2) begin
                audio_right = (b_clk) ? 16'h3FFF : 16'hC001;;
                audio_left = (c_clk) ? 16'h3FFF : 16'hC001;
            end
            else if(volume == 3) begin
                audio_right = (b_clk) ? 16'h5FFF : 16'hA001;
                audio_left = (c_clk) ? 16'h5FFF : 16'hA001;
            end
            else if(volume == 4) begin
                audio_right = (b_clk) ? 16'h7FFF : 16'h8001;;
                audio_left = (c_clk) ? 16'h7FFF : 16'h8001;
            end
            else begin
                audio_left = audio_left;
                audio_right = audio_right;
            end
        end
    end
endmodule

module SevenSegment(
	output reg [6:0] display,
	output reg [3:0] digit,
	input wire [15:0] nums,
	input wire rst,
	input wire clk
    );
    
    reg [15:0] clk_divider;
    reg [3:0] display_num;
    
    always @ (posedge clk, posedge rst) begin
    	if (rst) begin
    		clk_divider <= 15'b0;
    	end else begin
    		clk_divider <= clk_divider + 15'b1;
    	end
    end
    
    always @ (posedge clk_divider[15], posedge rst) begin
    	if (rst) begin
    		display_num <= 4'b0000;
    		digit <= 4'b1111;
    	end else begin
    		case (digit)
    			4'b1110 : begin
    					display_num <= nums[7:4];
    					digit <= 4'b1101;
    				end
    			4'b1101 : begin
						display_num <= nums[11:8];
						digit <= 4'b1011;
					end
    			4'b1011 : begin
						display_num <= nums[15:12];
						digit <= 4'b0111;
					end
    			4'b0111 : begin
						display_num <= nums[3:0];
						digit <= 4'b1110;
					end
    			default : begin
						display_num <= nums[3:0];
						digit <= 4'b1110;
					end				
    		endcase
    	end
    end
    
    always @ (*) begin
    	case (display_num)
    		`C : display = 7'b1000110;	//0000
			`D : display = 7'b0100001;   //0001                                                
			`E : display = 7'b0000110;   //0010                                                
			`F : display = 7'b0001110;   //0011                                             
			`G : display = 7'b0010000;   //0100                                               
			`A : display = 7'b0001000;   //0101                                               
			`B : display = 7'b0000011;   //0110
			`min : display = 7'b0111111;
			default: display = 7'b0111111;
    	endcase
    end
    
endmodule


module speaker(
    clk, // clock from crystal
    rst, // active high reset: BTNC
    _play, // SW: Play/Pause
    _mute, // SW: Mute
    _repeat, // SW: Repeat
    _music, // SW: Music
    _volUP, // BTN: Vol up
    _volDOWN, // BTN: Vol down
    _led_vol, // LED: volume
    audio_mclk, // master clock
    audio_lrck, // left-right clock
    audio_sck, // serial clock
    audio_sdin, // serial audio data input
    DISPLAY, // 7-seg
    DIGIT // 7-seg
);

    // I/O declaration
    input clk;  // clock from the crystal
    input rst;  // active high reset
    input _play, _mute, _repeat, _music, _volUP, _volDOWN;
    output [4:0] _led_vol;
    output audio_mclk; // master clock
    output audio_lrck; // left-right clock
    output audio_sck; // serial clock
    output audio_sdin; // serial audio data input
    output [6:0] DISPLAY;
    output [3:0] DIGIT;

    reg [2:0] volume = 3'd3;

    // Internal Signal
    wire [15:0] audio_in_left, audio_in_right, nums;
    
    wire clkDiv13, clkDiv16, clkDiv22;
    wire vol_up_db, vol_up_op, vol_down_db, vol_down_op;
    wire [11:0] ibeatNum; // Beat counter
    wire [31:0] freqL, freqR; // Raw frequency, produced by music module
    wire [21:0] freq_outL, freq_outR; // Processed Frequency, adapted to the clock rate of Basys3

    assign freq_outL = 50000000 / (_mute || !_play ? `sil : freqL);
    assign freq_outR = 50000000 / (_mute || !_play ? `sil : freqR);
    
    assign _led_vol = (_mute) ? 5'b0_0000 : (volume == 0) ? 5'b0_0001 : (volume == 1) ? 5'b0_0011 : (volume == 2) ? 5'b0_0111 : (volume == 3)  ? 5'b0_1111 : (volume == 4) ? 5'b1_1111 : 5'b0_0000  ;

    clock_divider #(.n(13)) clock_13(.clk(clk),.clk_div(clkDiv13));
    clock_divider #(.n(16)) clock_16(.clk(clk),.clk_div(clkDiv16));
    clock_divider #(.n(22)) clock_22(.clk(clk),.clk_div(clkDiv22));
    
    debounce bounce1(.pb_debounced(vol_up_db),.clk(clkDiv16),.pb(_volUP));
    debounce bounce2(.pb_debounced(vol_down_db),.clk(clkDiv16),.pb(_volDOWN));
    onepulse pulse1(.op(vol_up_op),.clk(clkDiv16),.signal(vol_up_db));
    onepulse pulse2(.op(vol_down_op),.clk(clkDiv16),.signal(vol_down_db));
    
    assign nums[15:4] = 12'b0111_0111_0111;
    assign nums[3:0] = (freqR % 262 == 0) ? 4'd0 : 
                       (freqR % 294 == 0) ? 4'd1 :
                       (freqR % 330 == 0) ? 4'd2 :
                       (freqR % 349 == 0) ? 4'd3 :
                       (freqR % 392 == 0 || freqR % 415 == 0) ? 4'd4 :
                       (freqR % 440 == 0) ? 4'd6 :
                       (freqR % 494 == 0) ? 4'd6 : 4'd7;
    
    SevenSegment ss(.clk(clk),.rst(rst) ,.nums(nums),.digit(DIGIT),.display(DISPLAY));
    
    // Player Control
    _player_control playerCtrl_00 ( 
        .clk(clkDiv22),
        .reset(rst),
        ._play(_play),
        ._repeat(_repeat),
        ._music(_music),
        .ibeat(ibeatNum)
    );

    // Music module
    // [in]  beat number and en
    // [out] left & right raw frequency
    music_example music_00 (
        .ibeatNum(ibeatNum),
        .en(1'd0),
        .toneL(freqL),
        .toneR(freqR)
    );

    // Note generation
    // [in]  processed frequency
    // [out] audio wave signal (using square wave here)
    note_gen noteGen_00(
        .clk(clk), // clock from crystal
        .rst(rst), // active high reset
        .note_div_left(freq_outL),
        .note_div_right(freq_outR),
        .audio_left(audio_in_left), // left sound audio
        .audio_right(audio_in_right),
        .volume(volume) // 3 bits for 5 levels
    );

    // Speaker controller
    speaker_control sc(
        .clk(clk),  // clock from the crystal
        .rst(rst),  // active high reset
        .audio_in_left(audio_in_left), // left channel audio data input
        .audio_in_right(audio_in_right), // right channel audio data input
        .audio_mclk(audio_mclk), // master clock
        .audio_lrck(audio_lrck), // left-right clock
        .audio_sck(audio_sck), // serial clock
        .audio_sdin(audio_sdin) // serial audio data input
    );

    always@(posedge clkDiv16)begin
        if(rst) begin
            volume = 3'd3;
        end
        else begin
            if(vol_up_op && volume < 4) begin
                volume = volume + 1;
            end
            else if(vol_down_op && volume > 0) begin
                volume = volume - 1;
            end
            else begin
                volume = volume;
            end
        end
    end

endmodule

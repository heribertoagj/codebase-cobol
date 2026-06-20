      ***************************************************************** 00000100
      * NOME DA INC - GFCTWAOU                                        * 00000200
      * DESCRICAO   - AREA DE COMUNICACAO - CONSULTA IRES             * 00000300
      * TAMANHO     - 130                                             * 00000400
      * DATA        - DEZ/2010                                        * 00000500
      * RESPONSAVEL - PAGNOCCA - SONDA PROCWORK                       * 00000600
      *================================================================*00000700
ST2507*               U L T I M A   A L T E R A C A O                *  00000710
      *----------------------------------------------------------------*00000720
ST2507*    ANALISTA....:  STEFANINI                                    *00000730
ST2507*    DATA........:  07 / 2025                                    *00000740
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *00000750
      *================================================================*00000760
      *                                                                 00000800
       01  GFCTWAOU-REG.                                                00000900
           10 WAOU-CHAVE-GFCT.                                          00001000
ST2507*       15 WAOU-CCGC-CPF             PIC  9(09).                  00001100
ST2507*       15 WAOU-CFLIAL-CGC           PIC  9(05).                  00001200
ST2507        15 WAOU-CCGC-CPF             PIC  X(09).                  00001210
ST2507        15 WAOU-CFLIAL-CGC           PIC  X(04).                  00001220
              15 WAOU-CCTRL-CPF-CGC        PIC  9(02).                  00001300
              15 WAOU-DSOLTC-CNST          PIC  X(10).                  00001400
              15 WAOU-BANCO                PIC  9(03).                  00001500
              15 WAOU-AGE-DEB              PIC  9(05).                  00001600
              15 WAOU-CTA-DEB              PIC  9(13).                  00001700
           10  WAOU-INF-IRES.                                           00001800
ST2507*       15 WAOU-CPF-CGC-NUM          PIC  9(09).                  00001900
ST2507*       15 WAOU-CPF-CGC-FILIAL       PIC  9(05).                  00002000
ST2507        15 WAOU-CPF-CGC-NUM          PIC  X(09).                  00002010
ST2507        15 WAOU-CPF-CGC-FILIAL       PIC  X(04).                  00002020
              15 WAOU-CPF-CTR              PIC  9(02).                  00002100
              15 WAOU-DTA-CONSULTA         PIC  X(10).                  00002200
              15 WAOU-RES-CONSULTA         PIC  X(01).                  00002300
              15 WAOU-TIPO-IMPED.                                       00002400
                 20 WAOU-TP-IMPED          PIC  9(03).                  00002500
                 20 WAOU-RS-IMPED          PIC  X(25).                  00002600
           10 WAOU-CHAVE-EVENTO.                                        00002700
              15 WAOU-CROTNA-ORIGE-MOVTO   PIC  X(04).                  00002800
              15 WAOU-DENVIO-MOVTO-TARIF   PIC  X(10).                  00002900
              15 WAOU-CNRO-ARQ-MOVTO       PIC  9(02).                  00003000
              15 WAOU-CSEQ-MOVTO           PIC  9(11) COMP-3.           00003100
              15 WAOU-CSERVC-TARIF         PIC  9(05) COMP-3.           00003200
           10 FILLER                       PIC  X(05).                  00003300
      *                                                                 00003400

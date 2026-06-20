      *                                                                 00000100
      ******************************************************************00000200
      **************************** GFCTWAOT ****************************00000300
      ******************************************************************00000400
      *                   IMAGEM DA TABELA GFCTB092                    *00000500
      *                   COM COMPLEMENTO DE TARIFA                    *00000600
      *                   CESTA CELULAR GESTAO                         *00000700
      *                   MAIS TARIFA, CLIENTE, ADESAO, OC. ERRO       *00000800
      * LRECL 0450 ======================================== 24.02.2011 *00000900
      *                                                                *00001000
      * PAGNOCCA         -     SONDA PROCWORK     -     GRUPO 50       *00001100
      *================================================================*00001200
ST2507*               U L T I M A   A L T E R A C A O                *  00001300
      *----------------------------------------------------------------*00001310
ST2507*    ANALISTA....:  STEFANINI                                    *00001320
ST2507*    DATA........:  07 / 2025                                    *00001330
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *00001340
      *================================================================*00001350
      *                                                                 00001400
       01  REG-GFCTWAOT.                                                00001500
           05  WAOT-DADOS-EVENTO.                                       00001600
               10 WAOT-CHAVE-EVENTO.                                    00001700
                  15 WAOT-CROTNA-ORIGE-MOVTO PIC  X(04).                00001800
                  15 WAOT-DENVIO-MOVTO-TARIF PIC  X(10).                00001900
                  15 WAOT-CNRO-ARQ-MOVTO     PIC S9(02)V        COMP-3. 00002000
                  15 WAOT-CSEQ-MOVTO         PIC S9(11)V        COMP-3. 00002100
               10 WAOT-CSERVC-TARIF          PIC S9(05)V        COMP-3. 00002200
               10 WAOT-CMIDIA-SERVC-MOVTO    PIC S9(05)V        COMP-3. 00002300
               10 WAOT-CPERIF-SERVC-MOVTO    PIC S9(05)V        COMP-3. 00002400
               10 WAOT-QEVNTO-AGRUP          PIC S9(06)V        COMP-3. 00002500
               10 WAOT-HPREST-SERVC-MOVTO    PIC  X(08).                00002600
               10 WAOT-CTPO-CTA-MOVTO        PIC S9(01)V        COMP-3. 00002700
               10 WAOT-CBCO-DSTNO-MOVTO      PIC S9(03)V        COMP-3. 00002800
               10 WAOT-CAG-DSTNO-MOVTO       PIC S9(05)V        COMP-3. 00002900
               10 WAOT-CCTA-DSTNO-MOVTO      PIC S9(13)V        COMP-3. 00003000
               10 WAOT-CPAB-DSTNO-MOVTO      PIC S9(05)V        COMP-3. 00003100
               10 WAOT-CAG-CTLZA-DEB         PIC S9(05)V        COMP-3. 00003200
               10 WAOT-CCTA-CTLZA-DEB        PIC S9(13)V        COMP-3. 00003300
               10 WAOT-VTARIF-BRUTO-MOVTO    PIC S9(09)V9(2)    COMP-3. 00003400
               10 WAOT-VTARIF-LIQ-MOVTO      PIC S9(09)V9(2)    COMP-3. 00003500
               10 WAOT-VTARIF-DEB-MOVTO      PIC S9(09)V9(2)    COMP-3. 00003600
               10 WAOT-DAGNDA-DEB-MOVTO      PIC  X(10).                00003700
               10 WAOT-DEFETV-DEB-MOVTO      PIC  X(10).                00003800
               10 WAOT-QMAX-TENTV-DEB        PIC S9(03)V        COMP-3. 00003900
               10 WAOT-QTENTV-DEB-MOVTO      PIC S9(03)V        COMP-3. 00004000
               10 WAOT-CSIT-EVNTO-RECBD      PIC S9(02)V        COMP-3. 00004100
               10 WAOT-CIDTFD-PGMC-COBR      PIC S9(02)V        COMP-3. 00004200
               10 WAOT-DOCOR-EVNTO           PIC  X(10).                00004300
               10 WAOT-VTARIF-CADTR-SIST     PIC S9(09)V9(2)    COMP-3. 00004400
               10 WAOT-CINDCD-TARIF-PCELD    PIC  X(1).                 00004500
               10 WAOT-QREG-INFMD-BONIF      PIC S9(5)V         COMP-3. 00004600
               10 WAOT-CINDCD-TARIF-EXCED    PIC S9(1)V         COMP-3. 00004700
               10 WAOT-CDOCTO-EXTRT          PIC S9(9)V         COMP-3. 00004800
               10 WAOT-DANO-REFT-EXTRT       PIC S9(4)V         COMP-3. 00004900
               10 WAOT-CFUNC-SOLCT-EXTRT     PIC S9(9)V         COMP-3. 00005000
               10 WAOT-CTERM                 PIC  X(8).                 00005100
               10 WAOT-RJUSTF-SOLTC-EXTRT    PIC  X(30).                00005200
           05  WAOT-DADOS-TARIFA-CEL.                                   00005300
               10 WAOT-VREPASS-CRED-CEL      PIC S9(09)V9(2)    COMP-3. 00005400
               10 WAOT-QTDIAS-CRED-REJ       PIC S9(05)V        COMP-3. 00005500
           05  WAOT-DADOS-ADESAO.                                       00005600
               10 WAOT-CSERVC-TARIF-ADES     PIC S9(05)V        COMP-3. 00005700
               10 WAOT-HINCL-REG             PIC  X(26).                00005800
               10 WAOT-DINIC-ADSAO-INDVD     PIC  X(10).                00005900
               10 WAOT-DFIM-ADSAO-INDVD      PIC  X(10).                00006000
           05  WAOT-DADOS-CELULAR.                                      00006100
               10 WAOT-NSEQ-REG-CLULR        PIC S9(05)V        COMP-3. 00006200
               10 WAOT-COPER-FONE-MOVEL      PIC S9(09)V        COMP-3. 00006300
               10 WAOT-CDDD-FONE-MOVEL       PIC S9(03)V        COMP-3. 00006400
               10 WAOT-CFONE-MOVEL           PIC S9(11)V        COMP-3. 00006500
           05  WAOT-DADOS-CLIENTE.                                      00006600
               10 WAOT-NOME-CLIE             PIC  X(40).                00006700
               10 WAOT-CGC-CPF.                                         00006800
ST2507*           15  WAOT-CNPJ-NUM          PIC  9(09)         COMP-3. 00006900
ST2507*           15  WAOT-CNPJ-FIL          PIC  9(05)         COMP-3. 00007000
ST2507            15  WAOT-CNPJ-NUM          PIC  X(09).                00007010
ST2507            15  WAOT-CNPJ-FIL          PIC  X(04).                00007020
                  15  WAOT-CTRL              PIC  9(02).                00007100
               10 WAOT-NRO-CEP               PIC  9(05)         COMP-3. 00007200
               10 WAOT-COMPL-CEP             PIC  9(03).                00007300
               10 WAOT-POSTO-SERV            PIC  9(03)         COMP-3. 00007400
               10 WAOT-RAMO-ATIV             PIC  9(05)         COMP-3. 00007500
               10 WAOT-DT-ABERT              PIC  9(09)         COMP-3. 00007600
               10 WAOT-DT-ULTMV              PIC  9(09)         COMP-3. 00007700
               10 WAOT-DT-NASC               PIC  9(09)         COMP-3. 00007800
               10 WAOT-SEXO                  PIC  X(01).                00007900
               10 WAOT-TP-CHEQ               PIC  X(01).                00008000
               10 WAOT-TP-EXTR               PIC  X(01).                00008100
           05  WAOT-OC-ERRO.                                            00008200
               10 WAOT-JOBNAME               PIC  X(08).                00008300
               10 WAOT-PGMNAME               PIC  X(08).                00008400
               10 WAOT-RECURSO               PIC  X(08).                00008500
               10 WAOT-DATAHORA              PIC  X(26).                00008600
               10 WAOT-COD-RETORNO           PIC  9(03).                00008700
               10 WAOT-DESC-OC               PIC  X(40).                00008800
           05  FILLER                        PIC  X(17).                00008900
      *                                                                 00009000
      **************************** GFCTWAOT ****************************00009100
      *                                                                 00009200

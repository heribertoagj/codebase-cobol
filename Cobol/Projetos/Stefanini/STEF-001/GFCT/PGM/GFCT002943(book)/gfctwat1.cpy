      *                                                                 00000100
      ***************************************************************** 00000200
      * NOME DA INC - GFCTWAT1                                        * 00000300
      * DESCRICAO   - LAYOUT PARA BATIMENTO DE REGISTROS DO GFCT      * 00000400
      *               COM O MORA                                      * 00000500
      * TAMANHO     - 0680                                            * 00000600
      * DATA        - 09.2011                                         * 00000700
      * RESPONSAVEL - PAGNOCCA - SONDA PROCWORK                       * 00000800
      *================================================================*00000900
ST2507*               U L T I M A   A L T E R A C A O                *  00000910
      *----------------------------------------------------------------*00000920
ST2507*    ANALISTA....:  STEFANINI                                    *00000930
ST2507*    DATA........:  07 / 2025                                    *00000940
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *00000950
      *================================================================*00000960
      *                                                                 00001000
       01  WAT1-REG.                                                    00001100
           05  WAT1-CHAVE.                                              00001200
              10  WAT1K-CROTNA-ORIGE-MOVTO   PIC  X(004).               00001300
              10  WAT1K-DENVIO-MOVTO-TARIF   PIC  X(010).               00001400
              10  WAT1K-CNRO-ARQ-MOVTO       PIC  9(002).               00001500
              10  WAT1K-CSEQ-MOVTO           PIC  9(011).               00001600
              10  WAT1K-DIG-CONTRATO         PIC  X(001).               00001700
              10  WAT1K-NUM-CONTRATO         PIC  9(007).               00001800
              10  WAT1K-CARTEIRA-DEB         PIC  9(005).               00001900
              10  WAT1K-VALOR-DEBITAR        PIC  9(011)V9(02).         00002000
           05  WAT1-INFO.                                               00002100
              10  WAT1I-TIPO-COBRANCA        PIC  9(002).               00002200
              10  WAT1I-TP-COBR-MORA         PIC  9(002).               00002300
              10  WAT1I-NUM-GRUPO            PIC  9(003).               00002400
              10  WAT1I-TOT-GRUPO            PIC  9(003).               00002500
              10  WAT11-VR-COBRADO           PIC  9(009)V9(02).         00002600
              10  WAT11-VR-DETALHE           PIC  9(009)V9(02).         00002700
              10  WAT1I-DT-PROC              PIC  9(008).               00002800
              10  WAT1I-CD-ACAO              PIC  9(002).               00002900
              10  WAT1I-CD-IDENT             PIC  9(015).               00003000
              10  WAT1I-CD-ERRO              PIC  9(003).               00003100
              10  WAT1I-SIT-92               PIC  9(001).               00003200
              10  WAT1I-SIT-COBR             PIC  9(001).               00003300
              10  WAT1I-SIT-MORA             PIC  9(001).               00003400
           05  WAT1-REG-92.                                             00003500
              10  WAT11-CROTNA-ORIGE-MOVTO   PIC  X(004).               00003600
              10  WAT11-DENVIO-MOVTO-TARIF   PIC  X(010).               00003700
              10  WAT11-CNRO-ARQ-MOVTO       PIC S9(002) COMP-3.        00003800
              10  WAT11-CSEQ-MOVTO           PIC S9(011) COMP-3.        00003900
              10  WAT11-CSERVC-TARIF         PIC S9(005) COMP-3.        00004000
              10  WAT11-CMIDIA-SERVC-MOVTO   PIC S9(005) COMP-3.        00004100
              10  WAT11-CPERIF-SERVC-MOVTO   PIC S9(005) COMP-3.        00004200
              10  WAT11-QEVNTO-AGRUP         PIC S9(006) COMP-3.        00004300
              10  WAT11-HPREST-SERVC-MOVTO   PIC  X(008).               00004400
              10  WAT11-CTPO-CTA-MOVTO       PIC S9(001) COMP-3.        00004500
              10  WAT11-CBCO-DSTNO-MOVTO     PIC S9(003) COMP-3.        00004600
              10  WAT11-CAG-DSTNO-MOVTO      PIC S9(005) COMP-3.        00004700
              10  WAT11-CCTA-DSTNO-MOVTO     PIC S9(013) COMP-3.        00004800
              10  WAT11-CPAB-DSTNO-MOVTO     PIC S9(005) COMP-3.        00004900
              10  WAT11-CAG-CTLZA-DEB        PIC S9(005) COMP-3.        00005000
              10  WAT11-CCTA-CTLZA-DEB       PIC S9(013) COMP-3.        00005100
              10  WAT11-VTARIF-BRUTO-MOVTO   PIC S9(009)V9(02) COMP-3.  00005200
              10  WAT11-VTARIF-LIQ-MOVTO     PIC S9(009)V9(02) COMP-3.  00005300
              10  WAT11-VTARIF-DEB-MOVTO     PIC S9(009)V9(02) COMP-3.  00005400
              10  WAT11-DAGNDA-DEB-MOVTO     PIC  X(010).               00005500
              10  WAT11-DEFETV-DEB-MOVTO     PIC  X(010).               00005600
              10  WAT11-QMAX-TENTV-DEB       PIC S9(003) COMP-3.        00005700
              10  WAT11-QTENTV-DEB-MOVTO     PIC S9(003) COMP-3.        00005800
              10  WAT11-CSIT-EVNTO-RECBD     PIC S9(002) COMP-3.        00005900
              10  WAT11-CIDTFD-PGMC-COBR     PIC S9(002) COMP-3.        00006000
              10  WAT11-DOCOR-EVNTO          PIC  X(010).               00006100
              10  WAT11-VTARIF-CADTR-SIST    PIC S9(009)V9(02) COMP-3.  00006200
              10  WAT11-CINDCD-TARIF-PCELD   PIC  X(001).               00006300
              10  WAT11-QREG-INFMD-BONIF     PIC S9(005) COMP-3.        00006400
              10  WAT11-CINDCD-TARIF-EXCED   PIC S9(001) COMP-3.        00006500
              10  WAT11-CDOCTO-EXTRT         PIC S9(009) COMP-3.        00006600
              10  WAT11-DANO-REFT-EXTRT      PIC S9(004) COMP-3.        00006700
              10  WAT11-CFUNC-SOLCT-EXTRT    PIC S9(009) COMP-3.        00006800
              10  WAT11-CTERM                PIC  X(008).               00006900
              10  WAT11-RJUSTF-SOLTC-EXTRT   PIC  X(030).               00007000
           05  WAT1-REG-COBR.                                           00007100
                10 WAT12-CHAVE-COBRANCA.                                00007200
                   15 WAT12-DATA-ENVIO         PIC 9(08).               00007300
                   15 WAT12-DIG-CONTRATO       PIC X(01).               00007400
                   15 WAT12-NUM-CONTRATO       PIC 9(07).               00007500
                10 WAT12-CHAVE-ORIGEM.                                  00007600
                   15 WAT12-DATA-ORIG          PIC X(10).               00007700
                   15 WAT12-ROTINA-ORIG        PIC X(04).               00007800
                   15 WAT12-NUM-ARQ-ORIG       PIC 9(02).               00007900
                   15 WAT12-SEQ-ARQ-ORIG       PIC 9(11).               00008000
                   15 WAT12-TARIFA-ORIG        PIC 9(05).               00008100
                10 WAT12-CGC-CPF.                                       00008200
ST2507*            15 WAT12-CGC-CPF-MUNERO     PIC 9(09).               00008300
ST2507*            15 WAT12-CGC-CPF-FILIAL     PIC 9(05).               00008400
ST2507             15 WAT12-CGC-CPF-MUNERO     PIC X(09).               00008410
ST2507             15 WAT12-CGC-CPF-FILIAL     PIC X(04).               00008420
                   15 WAT12-CGC-CPF-CONTROLE   PIC 9(02).               00008500
                10 WAT12-DADOS-DEBITO.                                  00008600
                   15 WAT12-BANCO-DEB          PIC 9(03).               00008700
                   15 WAT12-AGENCIA-DEB        PIC 9(05).               00008800
                   15 WAT12-CONTA-DEB          PIC 9(07).               00008900
                   15 WAT12-DIGITO-DEB         PIC X(01).               00009000
                   15 WAT12-CARTEIRA-DEB       PIC 9(05).               00009100
                   15 WAT12-COD-LCTO-DEB       PIC 9(05).               00009200
                   15 WAT12-COD-MOEDA-DEB      PIC 9(05).               00009300
                   15 WAT12-RAZAO.                                      00009400
                      20 WAT12-GRUPO-DEB       PIC 9(03).               00009500
                      20 WAT12-SUBGRUPO-DEB    PIC 9(03).               00009600
                   15 WAT12-VALOR-DEBITAR      PIC 9(11)V9(2).          00009700
                10 WAT12-DETALHES-COBRANCA.                             00009800
                   15 WAT12-SITUACAO-EVENTO    PIC 9(02).               00009900
                   15 WAT12-TIPO-COBRANCA      PIC 9(02).               00010000
                   15 WAT12-TIPO-ARQUIVO       PIC 9(02).               00010100
                   15 WAT12-DEB-PARCIAL        PIC 9(02).               00010200
                   15 WAT12-TIPO-SALDO         PIC 9(02).               00010300
                   15 WAT12-TIPO-CONTA         PIC 9(01).               00010400
                   15 WAT12-INCONDICIONAL      PIC X(01).               00010500
                   15 WAT12-ACEITA-PARCELADO   PIC X(01).               00010600
                   15 WAT12-PARCIAL-ULT-REPIQ  PIC X(01).               00010700
                   15 WAT12-VALOR-MINIMO       PIC 9(11)V9(2).          00010800
                   15 WAT12-BANCO-ORIG         PIC 9(03).               00010900
                   15 WAT12-AGENCIA-ORIG       PIC 9(05).               00011000
                   15 WAT12-DATA-EVNTO-ORIG    PIC 9(08).               00011100
                   15 WAT12-SEG-LINHA-EXTRATO  PIC X(32).               00011200
                10 WAT12-RETORNO.                                       00011300
                   15 WAT12-JOB                PIC X(08).               00011400
                   15 WAT12-PROGRAMA           PIC X(08).               00011500
                   15 WAT12-RECURSO            PIC X(08).               00011600
                   15 WAT12-COD-RETORNO        PIC X(03).               00011700
                   15 WAT12-VALOR-DEBITADO     PIC 9(11)V9(2).          00011800
                   15 WAT12-DESCRICAO          PIC X(50).               00011900
                10 WAT12-TRAG-EXT.                                      00012000
                   15 WAT12-CENTRALIZ          PIC X(01).               00012100
                   15 WAT12-AGENCIA-RET        PIC 9(05).               00012200
                   15 WAT12-CONTA-RET          PIC 9(07).               00012300
                10 WAT12-DOCTO-EXTRT           PIC 9(09) COMP-3.        00012400
                10 WAT12-NUM-PARC              PIC 9(03) COMP-3.        00012500
            05  WAT3-REG-MORA.                                          00012600
                10 WAT13-AGENCIA               PIC 9(05) COMP-3.        00012700
                10 WAT13-CONTA                 PIC 9(07) COMP-3.        00012800
                10 WAT13-CARTEIRA              PIC X(05).               00012900
                10 WAT13-CONTRATO              PIC 9(07) COMP-3.        00013000
                10 WAT13-DAC-CONTR             PIC 9(01).               00013100
                10 WAT13-DT-VCTO               PIC 9(09) COMP-3.        00013200
                10 WAT13-VR-PACELA             PIC 9(11)V99 COMP-3.     00013300
           05  FILLER                          PIC X(49).               00013400
      *                                                                 00013500
      ***************************************************************** 00013600

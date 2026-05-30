      *----------------------------------------------------------------*
      * BOOK I#BVVEVA     MOVTO VISANET - AGENDAMENTO PAGAMENTOS       *
      *                   RECEBIMENTO E RETORNO - DIARIO               *
      * TAMANHO : 120                                                  *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01         AGEND-REG.
      *                                                        001  120
         03       AGEND-CHAVE.
           05     AGEND-AGENC         PIC S9(05)  COMP-3.
      *                 ZEROS     - HEADER                     001  003
      *                 NUM.AGENC.- DETALHE
      *                 NOVES     - TRAILER
      *
           05     AGEND-DIGAG         PIC  X(01).
      *                 ZEROS     - HEADER                     004  001
      *                 NUM.AGENC.- DETALHE
      *                 NOVES     - TRAILER
      *
           05     AGEND-CONTA         PIC S9(09)  COMP-3.
      *                 ZEROS     - HEADER                     005  005
      *                 NUM.AGENC.- DETALHE
      *                 NOVES     - TRAILER
      *
           05     AGEND-DIGCC         PIC  X(01).
      *                 ZEROS     - HEADER                     010  001
      *                 NUM.AGENC.- DETALHE
      *                 NOVES     - TRAILER
      *
      *
         03       AGEND-REG-HEADER.
           05     AGEND-TIP-ARQ-HEAD  PIC 9(02).
      *                 02 - D+6 A D+30                        011  002
      *
           05     AGEND-NOM-ARQ-HEAD  PIC X(08).
      *                 BRANCOS - FIXO                         013  008
      *
           05     AGEND-DTMOV-HEAD    PIC S9(09)  COMP-3.
      *                 FORMATO AAAAMMDD                       021  005
      *
           05     AGEND-DTPROC-HEAD   PIC S9(09)  COMP-3.
      *                 FORMATO AAAAMMDD                       026  005
      *
           05     AGEND-DTINIPER-HEAD PIC S9(09)  COMP-3.
      *                 FORMATO AAAAMMDD                       031  005
      *
           05     AGEND-DTFINPER-HEAD PIC S9(09)  COMP-3.
      *                 FORMATO AAAAMMDD                       036  005
      *
           05     AGEND-RES-HEAD      PIC  X(69).
      *                                                        041  069
           05     AGEND-REM-BCO-HEAD  PIC S9(05)  COMP-3.
      *                                                        110  003
           05     AGEND-REM-VIS-HEAD  PIC S9(05)  COMP-3.
      *                                                        113  003
      *
      *
         03       AGEND-REG-DETALHE  REDEFINES  AGEND-REG-HEADER.
           05     AGEND-NUM-VEND-DET  PIC S9(11)  COMP-3.
      *                                                        011  006
           05     AGEND-DIG-VEND-DET  PIC  X(01).
      *                                                        017  001
           05     AGEND-NOM-VEND-DET  PIC  X(30).
      *                                                        018  030
           05     AGEND-NUM-RES-DET   PIC S9(07)  COMP-3.
      *                                                        048  004
           05     AGEND-DIG-RES-DET   PIC  X(01).
      *                                                        052  001
           05     AGEND-DTLANCV-DET   PIC S9(09)  COMP-3.
      *                 FORMATO AAAAMMDD                       053  005
      *
           05     AGEND-DTLANCB-DET   PIC S9(09)  COMP-3.
      *                 FORMATO AAAAMMDD                       058  005
      *
           05     AGEND-DEBCR-DET     PIC  9(01).
      *                 1 - CREDITO                            063  001
      *                 2 - DEBITO
      *
           05     AGEND-VLTRANS-DET   PIC S9(15)V99  COMP-3.
      *                                                        064  009
           05     AGEND-RES-DET       PIC  X(10).
      *                                                        073  010
           05     AGEND-MOEDA-DET     PIC  9(01).
      *                 1 - REAL                               083  001
      *                 2 - DOLAR
      *
           05     AGEND-CAPTURA-DET   PIC  9(01).
      *                 1 - SEM ANTECIPACAO                    084  001
      *                 2 - COM ANTECIPACAO
      *
           05     AGEND-OCORR-DET     PIC S9(03)  COMP-3.
      *                                                        085  002
           05     AGEND-RES1-DET      PIC  X(18).
      *                                                        087  018
           05     AGEND-CONT-VISA-DET PIC  X(11).
      *                                                        105  011
      *
      *
         03       AGEND-REG-TRAILER  REDEFINES  AGEND-REG-HEADER.
           05     AGEND-TOTCRE-TRAIL  PIC S9(15)V99  COMP-3.
      *                                                        011  009
           05     AGEND-TOTDEB-TRAIL  PIC S9(15)V99  COMP-3.
      *                                                        020  009
           05     AGEND-SOMVAL-TRAIL  PIC S9(15)V99  COMP-3.
      *                                                        029  009
           05     AGEND-RES-TRAIL     PIC  X(78).
      *                                                        038  078
         03       AGEND-NUM-SEQ       PIC S9(09)  COMP-3.
      *                                                        116  005

      *----------------------------------------------------------------*
      * BOOK I#BVVEVP     MOVTO VISANET - PAGTOS A FORNECEDORES        *
      *                   RECEBIMENTO E RETORNO - DIARIO               *
      * TAMANHO : 500                                                  *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01         PGTO-REG.
      *                                                        001  500
         03       PGTO-REG-HEADER.
           05     PGTO-COD-REG       PIC 9(01).
      *                0 - HEADER                              001  001
      *                1 - TRANSACAO
      *                9 - TRAILER
      *
           05     PGTO-COD-EMPR-HEAD PIC  9(08).
      *                                                        002  008
           05     PGTO-TIP-ID-HEAD   PIC  9(01).
      *                1 - CPF                                 010  001
      *                2 - CGC
      *
           05     PGTO-ID-HEAD.
             10   PGTO-CGC-NUM-HEAD  PIC  9(09).
      *                                                        011  009
             10   PGTO-CGC-FIL-HEAD  PIC  9(04).
      *                                                        020  004
             10   PGTO-CGC-DIG-HEAD  PIC  9(02).
      *                                                        024  002
           05     PGTO-NOME-HEAD     PIC  X(40).
      *                                                        026  040
           05     PGTO-TIP-SERV-HEAD PIC  9(02).
      *                20 - FIXO PARA CREDITO VENDEDOR         066  002
      *
           05     PGTO-COD-ORIG-HEAD PIC  9(01).
      *                1 - ORIGEM NO CLIENTE                   068  001
      *                2 - ORIGEM NO BANCO
      *
           05     PGTO-NUM-REM-HEQD  PIC  9(05).
      *                                                        069  005
           05     PGTO-NUM-RET-HEAD  PIC  9(05).
           05     PGTO-DTGRAV-HEAD   PIC  9(08).
      *                FORMATO AAAAMMDD                        079  008
      *
           05     PGTO-HRGRAV-HEAD   PIC  9(06).
      *                FORMATO HHMMSS                          087  006
      *
           05     PGTO-DENS-HEAD     PIC  X(05).
      *                                                        093  005
           05     PGTO-UNGRAV-HEQD   PIC  X(03).
      *                                                        098  003
           05     PGTO-ID-MICRO-HEAD PIC  X(05).
      *                                                        101  005
           05     PGTO-TIP-PROC-HEAD PIC  9(01).
      *                                                        106  001
           05     PGTO-RES-EMPR-HEAD PIC  X(74).
      *                                                        107  074
           05     PGTO-RES-BCO-HEAD  PIC  X(80).
      *                                                        181  080
           05     PGTO-RES-EXP-HEAD  PIC  X(234).
      *                                                        261  234
      *                                                        495  006
      *
      *
         03       PGTO-REG-TRANSACAO  REDEFINES  PGTO-REG-HEADER.
           05     FILLER             PIC  9(01).
      *                                                        001  001
           05     PGTO-FORNEC-TRAN.
             10   PGTO-TIP-ID-TRAN   PIC  9(01).
      *                1 - CPF                                 002  001
      *                2 - CGC
      *                3 - OUTROS
      *
             10   PGTO-ID-TRAN.
               15 PGTO-CGC-NUM-TRAN  PIC  9(009).
      *                                                        003  009
               15 PGTO-CGC-FIL-TRAN  PIC  9(004).
      *                                                        012  004
               15 PGTO-CGC-DIG-TRAN  PIC  9(002).
      *                                                        016  002
             10   PGTO-NOME-TRAN     PIC  X(30).
             10   PGTO-ENDE-TRAN     PIC  X(40).
      *                                                        048  040
             10   PGTO-CEP-TRAN      PIC  9(05).
      *                                                        088  005
             10   PGTO-CCEP-TRAN     PIC  9(03).
      *                                                        093  003
           05     PGTO-BANCO-TRAN.
             10   PGTO-COD-BCO-TRAN  PIC  9(03).
      *                                                        096  003
             10   PGTO-AGEN-TRAN     PIC  9(05).
      *                                                        099  005
             10   PGTO-DIGAG-TRAN    PIC  X(01).
      *                                                        104  001
             10   PGTO-CONTA-TRAN    PIC  9(13).
      *                                                        105  013
             10   PGTO-DIGCC-TRAN    PIC  X(02).
      *                                                        118  002
           05     PGTO-PAGTO-TRAN.
             10   PGTO-NUMPG-TRAN    PIC  X(19).
      *                                                        120  019
      *                                                        139  003
             10   PGTO-NOSSONN-TRAN  PIC  9(09).
      *                                                        142  009
             10   PGTO-SEUNUM-TRAN   PIC  X(15).
      *                                                        151  015
             10   PGTO-DTVENC-TRAN   PIC  9(08).
      *                FORMATO AAAAMMDD                        166  008
      *
             10   PGTO-DTEMIS-TRAN   PIC  9(08).
      *                FORMATO AAAAMMDD                        174  008
      *
             10   PGTO-DTLIM-TRAN    PIC  9(08).
      *                FORMATO AAAAMMDD                        182  008
      *
             10   PGTO-VLDOC-TRAN    PIC  9(13)V99.
      *                                                        190  015
             10   PGTO-VLPAG-TRAN    PIC  9(13)V99.
      *                                                        205  015
             10   PGTO-VLDESC-TRAN   PIC  9(13)V99.
      *                                                        220  015
      *                                                        235  015
             10   PGTO-TIP-DOC-TRAN  PIC  9(02).
      *                01 - N.F.FATURA                         250  002
      *                02 - FATURA
      *                03 - N.FISCAL
      *                04 - DUPLICATA
      *                05 - OUTROS
      *
             10   PGTO-NUM-DOC-TRAN  PIC  9(10).
      *                                                        252  010
             10   PGTO-SER-DOC-TRAN  PIC  X(02).
      *                                                        262  002
             10   PGTO-MOD-PAG-TRAN  PIC  9(02).
      *                01 - CREDITO C/C                        264  002
      *                02 - CHEQUE OP
      *                03 - DOC
      *                30 - TITULO BRADESCO
      *                31 - TITULO TERCEIROS
      *
             10   PGTO-DTEFPG-TRAN   PIC  9(08).
      *
             10   PGTO-COD-MOED-TRAN PIC  X(03).
      *                                                        274  003
           05     PGTO-CONTR-TRAN.
             10   PGTO-SIT-AGDM-TRAN PIC  X(02).
      *                01 - NAO PAGO                           277  002
      *                02 - PAGO
      *                03 - BLOQUEADO
      *                04 - CANCELADO
      *                05 - BX.COBR.S/PGTO.
      *                06 - BX.COBR.C/PGTO.
      *                07 - C/INSTR.PROTESTO
      *                08 - TRANSF.P/CARTORIO
      *                09 - BX.PELO DESCONTO
      *                10 - TIT.TERC.DEVOLV.
      *                11 - ESTORNADO
      *                12 - DOC EMITIDO
      *                22 - CHEQUE OP EMITIDO
      *
             10   PGTO-INF-RET1-TRAN PIC  X(02).
             10   PGTO-INF-RET2-TRAN PIC  X(02).
      *                                                        281  002
             10   PGTO-INF-RET3-TRAN PIC  X(02).
      *                                                        283  002
             10   PGTO-INF-RET4-TRAN PIC  X(02).
      *                                                        285  002
             10   PGTO-INF-RET5-TRAN PIC  X(02).
      *                                                        287  002
             10   PGTO-TIP-MOV-TRAN  PIC  X(01).
      *                0 - INCLUSAO      REMESSA               289  001
      *                5 - ALTERACAO       "
      *                9 - EXCLUSAO        "
      *
      *                1 - INCLUSAO TIT.CART.  RETORNO
      *                2 - ALTERAC. TIT.CART.    "
      *                3 - BAIXA    TIT.CART     "
      *
             10   PGTO-COD-MOV-TRAN  PIC  X(02).
      *                00 - AUTOR. AGENDAM.                    290  002
      *                25 - DESAUTOR. AGENDAM.
             10   PGTO-ENDSAC-TRAN   PIC  X(40).
      *                                                        292  040
             10   PGTO-AVALSAC-TRAN  PIC  X(40).
      *                                                        332  040
             10   PGTO-RES-TRAN      PIC  X(01).
      *                                                        372  001
             10   PGTO-NIVRET-TRAN   PIC  X(01).
      *                                                        373  001
             10   PGTO-INFCOMPL-TRAN PIC  X(40).
      *                                                        374  040
             10   PGTO-COD-AREA-TRAN PIC  X(02).
      *                                                        414  002
             10   PGTO-INF-EMPR-TRAN PIC  X(35).
      *                                                        416  035
             10   PGTO-RES1-TRAN     PIC  X(28).
      *                                                        451  028
             10   PGTO-TIP-CT-TRAN   PIC  9(01).
      *                                                        479  001
             10   PGTO-CT-COMPL-TRAN PIC  9(07).
      *                                                        480  007
      *                                                        487  008
             10   FILLER             PIC  X(06).
      *                                                        495  006
      *
      *
         03       PGTO-REG-TRAILER    REDEFINES  PGTO-REG-HEADER.
           05     FILLER             PIC  9(01).
      *                                                        001  001
           05     PGTO-QTREG-TRAIL   PIC  9(06).
      *                                                        002  006
           05     PGTO-TOTVAL-TRAIL  PIC  9(15)V99.
      *                                                        008  017
           05     PGTO-RESERV-TRAIL  PIC  X(470).
      *                                                        025  470
           05     FILLER             PIC  9(06).
      *                                                        495  006

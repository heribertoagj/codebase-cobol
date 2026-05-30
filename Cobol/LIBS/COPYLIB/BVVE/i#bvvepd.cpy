      *================================================================*
      *================================================================*
      *=         ESTA INC ESTA EM CONFORMIDADE COM O ANO 2000.        =*
      *=         DATA:  03/05/1999                                    =*
      *=         CPM   -   KIT2000                                    =*
      *================================================================*
      *================================================================*
      *----------------------------------------------------------------*
      * BOOK I#BVVEPD     MOVTO VENDEDOR - PAGAMENTO DOC               *
      *                                                                *
      * TAMANHO : 102                                                  *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01         PDOC-REG.
      *                                                        001  102
           05     PDOC-NUM-VEND.
             10   PDOC-PREF-VEND     PIC  9(05)  COMP-3.
      *                                                        001  003
             10   PDOC-NUM1-VEND     PIC  9(05)  COMP-3.
             10   PDOC-FIXO-VEND     PIC  9(01).
      *                                                        007  001
             10   PDOC-NUM2-VEND     PIC  9(07)  COMP-3.
      *                                                        008  004
           05     PDOC-NUM-LOTE      PIC  9(13)  COMP-3.
      *                                                        012  007
           05     PDOC-TIP-REG       PIC  9(01).
      *                                                        019  001
           05     PDOC-VLTRANS       PIC  9(11)V99  COMP-3.
      *                                                        020  007
CPMCPM     05     PDOC-DTRECEB       PIC  9(09)  COMP-3.
CPMCPM*                FORMATO DDMMSSAA                        027  005
      *
CPMCPM     05     PDOC-DTCRED        PIC  9(09)  COMP-3.
CPMCPM*                FORMATO DDMMSSAA                        032  005
      *
CPMCPM     05     PDOC-DTMOVTO       PIC  9(09)  COMP-3.
CPMCPM*                FORMATO DDMMSSAA                        037  005
      *
CPMCPM     05     PDOC-DTATUA        PIC  9(09)  COMP-3.
      *
           05     PDOC-NUM-DIAS      PIC  9(03)  COMP-3.
      *                                                        047  002
           05     PDOC-AGENC         PIC  9(05)  COMP-3.
      *                                                        049  003
           05     PDOC-RAZAO         PIC  9(05)  COMP-3.
      *                                                        052  003
           05     PDOC-CONTA         PIC  9(07)  COMP-3.
      *                                                        055  004
           05     PDOC-NOM-VEND      PIC  X(35).
      *                                                        059  035
           05     PDOC-TIP-PGTO      PIC  9(01).
      *                                                        094  001
           05     PDOC-TIP-CRED      PIC  X(01).
      *                                                        095  001
           05     PDOC-VLCOMISS      PIC  9(11)V99  COMP-3.
      *                                                        096  007

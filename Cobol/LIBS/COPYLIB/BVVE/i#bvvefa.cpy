      *================================================================*
      *================================================================*
      *=         ESTA INC ESTA EM CONFORMIDADE COM O ANO 2000.        =*
      *=         DATA:  03/05/1999                                    =*
      *=         CPM   -   KIT2000                                    =*
      *================================================================*
      *================================================================*
      *----------------------------------------------------------------*
      * BOOK I#BVVEFA     MOVTO VENDEDOR - FATURA ABERTA               *
      *                   AGENDAMENTO DIARIO                           *
      * TAMANHO : 039                                                  *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01         FATA-REG.
      *                                                        001  039
           05     FATA-AGENC         PIC  9(05)  COMP-3.
      *                                                        001  003
           05     FATA-CONTA         PIC  9(07)  COMP-3.
      *                                                        004  004
CPMCPM     05     FATA-DTCRED        PIC  9(07)  COMP-3.
CPMCPM*                FORMATO SSAADDD                         008  004
      *
           05     FATA-NUM-VEND      PIC  9(13)  COMP-3.
      *                                                        012  007
           05     FATA-TIP-REG       PIC  9(01).
      *                5 - A VISTA  U$                         019  001
      *                6 - A VISTA  R$
      *                8 - PARCEL.  U$
      *                9 - PARCEL.  R$
      *
           05     FATA-VL-PRINC      PIC  9(11)V99  COMP-3.
      *                                                        020  007
           05     FATA-VL-OUTROS     PIC  9(11)V99  COMP-3.
      *                                                        027  007
           05     FATA-VL-COMISS     PIC  9(09)V99  COMP-3.
      *                                                        034  006

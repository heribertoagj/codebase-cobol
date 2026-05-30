      *----------------------------------------------------------------*
      * BOOK I#BVVERC     REGISTRO DE CONTROLE DE REMESSA DO           *
      *                   ARQUIVO DE AGENDAMENTO DA REDECARD           *
      * TAMANHO : 050                                                  *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01         REM-REG.
      *                                                        001  020
           05     REM-NUMSEQ-ANT     PIC  9(06).
      *                                                        001  006
           05     REM-NUMSEQ-ATU     PIC  9(06).
      *                                                        007  012
           05     REM-DTREMES-ANT    PIC  9(08).
      *                FORMATO SSAAMMDD                        013  020
      *
           05     REM-DTREMES-ATU    PIC  9(08).
      *                FORMATO SSAAMMDD                        021  028
      *
           05     FILLER             PIC  X(22).

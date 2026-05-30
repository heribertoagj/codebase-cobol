      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB083: DB2PRD.TTPO_DESC_COML                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM483S                                       *
      *    -> FETCH   - DCOM483F                                       *
      *    -> INSERT  - DCOM483I                                       *
      *    -> UPDATE  - DCOM483U                                       *
      *    -> DELETE  - DCOM483D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW83S-HEADER.
          10 DCOMW83S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW83S'.
          10 DCOMW83S-TAM-LAYOUT   PIC  9(005) VALUE 06626.
      *
          10 DCOMW83S-BLOCO-SAIDA.
             15 DCOMW83S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW83S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW83S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMW83S-CTPO-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMW83S-ITPO-DESC-COML     PIC  X(030).
                20 DCOMW83S-IRSUMO-TPO-DESC    PIC  X(010).
                20 DCOMW83S-CSGL-TPO-DESC-COML PIC  X(006).
                20 DCOMW83S-HULT-ATULZ         PIC  X(026).
                20 DCOMW83S-CTRANS-PROG-ATULZ  PIC  X(008).
                20 DCOMW83S-FILLER             PIC  X(050).
      *

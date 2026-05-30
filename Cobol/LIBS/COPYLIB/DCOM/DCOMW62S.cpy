      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB062: DB2PRD.TPERDC_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM462S                                       *
      *    -> FETCH   - DCOM462F                                       *
      *    -> INSERT  - DCOM462I                                       *
      *    -> UPDATE  - DCOM462U                                       *
      *    -> DELETE  - DCOM462D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW62S-HEADER.
          10 DCOMW62S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW62S'.
          10 DCOMW62S-TAM-LAYOUT       PIC  9(005) VALUE 04376.
      *
          10 DCOMW62S-BLOCO-SAIDA.
             15 DCOMW62S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW62S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW62S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW62S-CTPO-ISENC-DESC        PIC S9(003) COMP-3.
                20 DCOMW62S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMW62S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW62S-DINIC-VGCIA-ISENC      PIC  X(010).
                20 DCOMW62S-DFIM-VGCIA-ISENC       PIC  X(010).
                20 DCOMW62S-CINDCD-REST-VLR        PIC  X(001).
                20 DCOMW62S-VREST-MAX-OPER         PIC S9(015)V9(02).
                20 DCOMW62S-HULT-ATULZ             PIC  X(026).
                20 DCOMW62S-CFUNC-BDSCO            PIC S9(008) COMP-3.
                20 DCOMW62S-CTERM                  PIC  X(008).
      *                                                                *
          10 DCOMW62S-FILLER                       PIC  X(200).
      *                                                                *

      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0D4: DB2PRD.TPSSOA_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4D4S                                       *
      *    -> FETCH   - DCOM4D4F                                       *
      *    -> INSERT  - DCOM4D4I                                       *
      *    -> UPDATE  - DCOM4D4U                                       *
      *    -> DELETE  - DCOM4D4D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWD4S-HEADER.
          10 DCOMWD4S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMWD4S'.
          10 DCOMWD4S-TAM-LAYOUT   PIC  9(005) VALUE 07500.
      *
          10 DCOMWD4S-BLOCO-SAIDA.
             15 DCOMWD4S-QTDE-TOTAL            PIC  9(010).
             15 DCOMWD4S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMWD4S-OCORRENCIA            OCCURS 050 TIMES.
                20 DCOMWD4S-NPSSOA-DESC-COML   PIC  S9(009) COMP-3.
                20 DCOMWD4S-CCNPJ-CPF          PIC  S9(009) COMP-3.
                20 DCOMWD4S-CFLIAL-CNPJ        PIC  S9(005) COMP-3.
                20 DCOMWD4S-CCTRL-CNPJ-CPF     PIC  S9(002) COMP-3.
                20 DCOMWD4S-CDOCTO-ID          PIC  X(015).
                20 DCOMWD4S-IPSSOA-DESC-COML   PIC  X(060).
                20 DCOMWD4S-CCLUB              PIC  S9(010) COMP-3.
                20 DCOMWD4S-DINCL-REG          PIC  X(010).
                20 DCOMWD4S-HULT-ATULZ         PIC  X(026).
                20 DCOMWD4S-CFUNC-BDSCO        PIC  S9(009) COMP-3.
                20 DCOMWD4S-CTERM              PIC  X(008).
                20 DCOMWD4S-CINDCD-TPO-PSSOA   PIC  X(001).
             15 DCOMWD4S-FILLER                PIC  X(174).
      *

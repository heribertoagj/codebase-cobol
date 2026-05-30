      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMWD4E-HEADER.
          10 DCOMWD4E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWD4E'.
          10 DCOMWD4E-TAM-LAYOUT       PIC  9(005) VALUE 00654.
      *
       05 DCOMWD4E-BLOCO-ENTRADA.
          10 DCOMWD4E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWD4E-ENTRADA.
             15 DCOMWD4E-NPSSOA-DESC-COML          PIC  S9(009) COMP-3.
             15 DCOMWD4E-CCNPJ-CPF                 PIC  S9(009) COMP-3.
             15 DCOMWD4E-CFLIAL-CNPJ               PIC  S9(005) COMP-3.
             15 DCOMWD4E-CCTRL-CNPJ-CPF            PIC  S9(002) COMP-3.
             15 DCOMWD4E-CDOCTO-ID                 PIC  X(015).
             15 DCOMWD4E-IPSSOA-DESC-COML          PIC  X(060).
             15 DCOMWD4E-CCLUB                     PIC  S9(010) COMP-3.
             15 DCOMWD4E-DINCL-REG                 PIC  X(010).
             15 DCOMWD4E-HULT-ATULZ                PIC  X(026).
             15 DCOMWD4E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMWD4E-CTERM                     PIC  X(008).
             15 DCOMWD4E-CINDCD-TPO-PSSOA          PIC  X(001).
      *
          10 DCOMWD4E-BLOCO-VARIAVEL.
             15 DCOMWD4E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWD4E-BLOCO-PAGINACAO.
             15 DCOMWD4E-MAX-OCORR                 PIC  9(003).
             15 DCOMWD4E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWD4E-P-INICIAL              VALUE 'I'.
                88 DCOMWD4E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWD4E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWD4E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWD4E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWD4E-BLOCO-RESTART.
             15 DCOMWD4E-CHAVE-INI.
                20 DCOMWD4E-I-NPSSOA-DESC-COML     PIC  S9(009) COMP-3.
                20 DCOMWD4E-I-CCNPJ-CPF            PIC  S9(009) COMP-3.
                20 DCOMWD4E-I-CFLIAL-CNPJ          PIC  S9(005) COMP-3.
                20 DCOMWD4E-I-CCTRL-CNPJ-CPF       PIC  S9(002) COMP-3.
                20 DCOMWD4E-I-CCLUB                PIC  S9(010) COMP-3.
                20 DCOMWD4E-I-CINDCD-TPO-PSSOA     PIC  X(001).
             15 DCOMWD4E-CHAVE-FIM.
                20 DCOMWD4E-F-NPSSOA-DESC-COML     PIC  S9(009) COMP-3.
                20 DCOMWD4E-F-CCNPJ-CPF            PIC  S9(009) COMP-3.
                20 DCOMWD4E-F-CFLIAL-CNPJ          PIC  S9(005) COMP-3.
                20 DCOMWD4E-F-CCTRL-CNPJ-CPF       PIC  S9(002) COMP-3.
                20 DCOMWD4E-F-CCLUB                PIC  S9(010) COMP-3.
                20 DCOMWD4E-F-CINDCD-TPO-PSSOA     PIC  X(001).
          10 DCOMWD4E-FILLER                       PIC  X(137).
      *                                                                *

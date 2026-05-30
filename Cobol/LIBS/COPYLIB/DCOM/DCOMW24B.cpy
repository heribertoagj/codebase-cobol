      ******************************************************************
      * NOME BOOK : DCOMW24B - BOOK DE INTERFACE COM SERVICO DCOM424B  *
      * DESCRICAO : CONSULTA INFORMACOES                               *
      * DATA      : 04/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 00204 BYTES                                        *
      *----------------------------------------------------------------*
      * DCOMW24B-E-TPO-PESQUISA = 1 - PESQUISA ORIGINAL TOTAL          *
      *                           2 - PESQUISA ORIGINAL PARCIAL-01     *
      *                           3 - PESQUISA ORIGINAL PARCIAL-02     *
      *                           4 - PESQUISA ORIGINAL PARCIAL-03     *
      ******************************************************************
           05 DCOMW24B-HEADER.
             10 DCOMW24B-COD-LAYOUT      PIC X(08) VALUE 'DCOMW24B'.
             10 DCOMW24B-TAM-LAYOUT      PIC 9(05) VALUE 00204.
           05 DCOMW24B-REGISTRO.
             10 DCOMW24B-AREA-ENTRADA.
                15 DCOMW24B-E-TPO-PESQUISA            PIC 9(002).
                15 DCOMW24B-E-NPSSOA-DESC-COML        PIC 9(009).
                15 DCOMW24B-E-CCNPJ-CPF               PIC 9(009).
                15 DCOMW24B-E-CFLIAL-CNPJ             PIC 9(005).
                15 DCOMW24B-E-CCTRL-CNPJ-CPF          PIC 9(002).
             10 DCOMW24B-AREA-SAIDA.
                15 DCOMW24B-S-NPSSOA-DESC-COML        PIC 9(009).
                15 DCOMW24B-S-CCNPJ-CPF               PIC 9(009).
                15 DCOMW24B-S-CFLIAL-CNPJ             PIC 9(005).
                15 DCOMW24B-S-CCTRL-CNPJ-CPF          PIC 9(002).
                15 DCOMW24B-S-CDOCTO-ID               PIC X(015).
                15 DCOMW24B-S-IPSSOA-DESC-COML        PIC X(060).
                15 DCOMW24B-S-CCLUB                   PIC 9(010).
                15 DCOMW24B-S-DINCL-REG               PIC X(010).
                15 DCOMW24B-S-HULT-ATULZ              PIC X(026).
                15 DCOMW24B-S-CFUNC-BDSCO             PIC 9(009).
                15 DCOMW24B-S-CTERM                   PIC X(008).
                15 DCOMW24B-S-CINDCD-TPO-PSSOA        PIC X(001).

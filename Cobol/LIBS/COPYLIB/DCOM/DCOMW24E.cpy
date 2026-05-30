      ******************************************************************
      * NOME BOOK : DCOMW24E - BOOK DE INTERFACE COM SERVICO DCOM424E  *
      * DESCRICAO : CONSULTA INFORMACOES SUBPRODUTO CONTRATO LIMITE    *
      * DATA      : 04/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 03124 BYTES                                        *
      ******************************************************************
           05 DCOMW24E-HEADER.
             10 DCOMW24E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW24E'.
             10 DCOMW24E-TAM-LAYOUT      PIC 9(05) VALUE 03124.
           05 DCOMW24E-REGISTRO.
             10 DCOMW24E-AREA-ENTRADA.
                15 DCOMW24E-E-MAX-OCORR               PIC 9(003).
                15 DCOMW24E-E-CCONTR-LIM-DESC         PIC 9(009).
                15 DCOMW24E-E-CVRSAO-CONTR-LIM        PIC 9(003).
                15 DCOMW24E-E-CPRODT                  PIC 9(003).
                15 DCOMW24E-E-CSPROD-DESC-COML        PIC 9(003).
             10 DCOMW24E-AREA-PAGINACAO.
                15 DCOMW24E-INDICADOR-PAGINACAO       PIC  X(001).
                   88 DCOMW24E-I-ANTERIOR             VALUE 'A'.
                   88 DCOMW24E-I-INICIAL              VALUE 'I'.
                   88 DCOMW24E-I-PRIMEIRA             VALUE 'P'.
                   88 DCOMW24E-I-SEGUINTE             VALUE 'S'.
                   88 DCOMW24E-I-ULTIMA               VALUE 'U'.
                15 DCOMW24E-BLOCO-RESTART.
                   20 DCOMW24E-PRI-CHAVE.
                      25 DCOMW24E-P-CCONTR-LIM-DESC   PIC 9(009).
                      25 DCOMW24E-P-CVRSAO-CONTR-LIM  PIC 9(003).
                      25 DCOMW24E-P-CPRODT            PIC 9(003).
                      25 DCOMW24E-P-CSPROD-DESC-COML  PIC 9(003).
                   20 DCOMW24E-ULT-CHAVE.
                      25 DCOMW24E-U-CCONTR-LIM-DESC   PIC 9(009).
                      25 DCOMW24E-U-CVRSAO-CONTR-LIM  PIC 9(003).
                      25 DCOMW24E-U-CPRODT            PIC 9(003).
                      25 DCOMW24E-U-CSPROD-DESC-COML  PIC 9(003).
             10 DCOMW24E-AREA-SAIDA.
                15 DCOMW24E-S-TOT-REG                 PIC 9(003).
                15 DCOMW24E-S-LISTA-SAIDA    OCCURS 050 TIMES.
                   25 DCOMW24E-S-CCONTR-LIM-DESC      PIC 9(009).
                   25 DCOMW24E-S-CVRSAO-CONTR-LIM     PIC 9(003).
                   25 DCOMW24E-S-CPRODT               PIC 9(003).
                   25 DCOMW24E-S-CSPROD-DESC-COML     PIC 9(003).
                   25 DCOMW24E-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW24E-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW24E-S-CTERM                PIC X(008).

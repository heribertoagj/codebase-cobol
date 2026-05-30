      ******************************************************************
      * NOME BOOK : DCOMW24G - BOOK DE INTERFACE COM SERVICO DCOM424G  *
      * DESCRICAO : CONSULTA INFORMACOES SUBPRODUTO CONTRATO LIMITE    *
      * DATA      : 05/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 02988 BYTES                                        *
      ******************************************************************
           05 DCOMW24G-HEADER.
             10 DCOMW24G-COD-LAYOUT      PIC X(08) VALUE 'DCOMW24G'.
             10 DCOMW24G-TAM-LAYOUT      PIC 9(05) VALUE 02988.
           05 DCOMW24G-REGISTRO.
             10 DCOMW24G-AREA-ENTRADA.
                15 DCOMW24G-E-MAX-OCORR               PIC 9(003).
                15 DCOMW24G-E-CPRODT                  PIC 9(003).
                15 DCOMW24G-E-CTPO-DESC-COML          PIC 9(003).
             10 DCOMW24G-AREA-PAGINACAO.
                15 DCOMW24G-INDICADOR-PAGINACAO       PIC  X(001).
                   88 DCOMW24G-I-ANTERIOR             VALUE 'A'.
                   88 DCOMW24G-I-INICIAL              VALUE 'I'.
                   88 DCOMW24G-I-PRIMEIRA             VALUE 'P'.
                   88 DCOMW24G-I-SEGUINTE             VALUE 'S'.
                   88 DCOMW24G-I-ULTIMA               VALUE 'U'.
                15 DCOMW24G-BLOCO-RESTART.
                   20 DCOMW24G-PRI-CHAVE.
                      25 DCOMW24G-P-CPRODT            PIC 9(003).
                      25 DCOMW24G-P-CTPO-DESC-COML    PIC 9(003).
                   20 DCOMW24G-ULT-CHAVE.
                      25 DCOMW24G-U-CPRODT            PIC 9(003).
                      25 DCOMW24G-U-CTPO-DESC-COML    PIC 9(003).
             10 DCOMW24G-AREA-SAIDA.
                15 DCOMW24G-S-TOT-REG                 PIC 9(003).
                15 DCOMW24G-S-LISTA-SAIDA    OCCURS 050 TIMES.
                   25 DCOMW24G-S-CPRODT               PIC 9(003).
                   25 DCOMW24G-S-CTPO-DESC-COML       PIC 9(003).
                   25 DCOMW24G-S-CDOCTO-FORML-DESC    PIC X(010).
                   25 DCOMW24G-S-HULT-ATULZ           PIC X(026).
                   25 DCOMW24G-S-CFUNC-BDSCO          PIC 9(009).
                   25 DCOMW24G-S-CTERM                PIC X(008).

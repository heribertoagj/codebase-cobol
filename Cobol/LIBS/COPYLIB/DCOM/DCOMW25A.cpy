      ******************************************************************
      * NOME BOOK : DCOMW25A - BOOK DE INTERFACE COM SERVICO DCOM425A  *
      * DESCRICAO : CONSULTA PARAMETRIZACAO CUSTO EFETIVO              *
      * DATA      : 16/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 061 BYTES                                          *
      ******************************************************************
       05 DCOMW25A-HEADER.
         10 DCOMW25A-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW25A'.
         10 DCOMW25A-TAM-LAYOUT        PIC 9(05)   VALUE 00061.
       05 DCOMW25A-REGISTRO.
         10 DCOMW25A-BLOCO-ENTRADA.
           15 DCOMW25A-E-CPRODT                    PIC 9(03).
           15 DCOMW25A-E-CSPROD-DESC-COML          PIC 9(03).
         10 DCOMW25A-BLOCO-SAIDA.
           15 DCOMW25A-S-CPRODT                    PIC 9(03).
           15 DCOMW25A-S-CSPROD-DESC-COML          PIC 9(03).
           15 DCOMW25A-S-CINDCD-PSSOA-TOMAD        PIC X(01).
           15 DCOMW25A-S-CFUNC-BDSCO               PIC 9(09).
           15 DCOMW25A-S-HATULZ                    PIC X(26).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************

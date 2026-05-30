      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  66-ARGUMENTOS-ENTRADA.

       05  66-COLUNAS-DA-TABELA.
           10  66-CPRODT                   PIC S9(3)V COMP-3.
           10  66-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  66-CINDCD-VCTO-PCELA        PIC X(1).
           10  66-CMEIO-PGTO-DESC          PIC S9(3)V COMP-3.
           10  66-HINIC-MEIO-PGTO          PIC X(26).
           10  66-CINDCD-RESP-PGTO         PIC X(1).
           10  66-TDEB-PCELA-VENCD         PIC S9(3)V COMP-3.
           10  66-CINDCD-PRZ-PCELA         PIC X(2).
           10  66-HFIM-MEIO-PGTO           PIC X(26).
           10  66-HULT-ATULZ               PIC X(26).
           10  66-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  66-CTERM                    PIC X(8).
           10  66-CINDCD-PGTO-OPCAO        PIC X(1).
           10  66-CINDCD-OPCAO-BLETO       PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  66-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  66-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  66-RETORNO.
       05  66-COD-RETORNO                         PIC 9(04).
       05  66-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  66-PROXIMO-RESTART                     PIC 9(05).
       05  66-QTDE-RETORNADA                      PIC 9(03).
       05  66-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  66-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T66.
           10  66RT-CPRODT                 PIC S9(3)V COMP-3.
           10  66RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  66RT-CINDCD-VCTO-PCELA      PIC X(1).
           10  66RT-CMEIO-PGTO-DESC        PIC S9(3)V COMP-3.
           10  66RT-HINIC-MEIO-PGTO        PIC X(26).
           10  66RT-CINDCD-RESP-PGTO       PIC X(1).
           10  66RT-TDEB-PCELA-VENCD       PIC S9(3)V COMP-3.
           10  66RT-CINDCD-PRZ-PCELA       PIC X(2).
           10  66RT-HFIM-MEIO-PGTO         PIC X(26).
           10  66RT-HULT-ATULZ             PIC X(26).
           10  66RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  66RT-CTERM                  PIC X(8).
           10  66RT-CINDCD-PGTO-OPCAO      PIC X(1).
           10  66RT-CINDCD-OPCAO-BLETO     PIC X(1).

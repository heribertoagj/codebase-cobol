      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  75-ARGUMENTOS-ENTRADA.

       05  75-COLUNAS-DA-TABELA.
           10  75-CCONTR-LIM-DESC          PIC S9(9)V COMP-3.
           10  75-CVRSAO-CONTR-LIM         PIC S9(3)V COMP-3.
           10  75-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  75-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  75-CFLIAL-CNPJ              PIC S9(5)V COMP-3.
           10  75-CCTRL-CNPJ-CPF           PIC S9(2)V COMP-3.
           10  75-HULT-ATULZ               PIC X(26).
           10  75-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  75-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  75-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  75-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  75-RETORNO.
       05  75-COD-RETORNO                         PIC 9(04).
       05  75-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  75-PROXIMO-RESTART                     PIC 9(05).
       05  75-QTDE-RETORNADA                      PIC 9(03).
       05  75-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  75-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T75.
           10  75RT-CCONTR-LIM-DESC        PIC S9(9)V COMP-3.
           10  75RT-CVRSAO-CONTR-LIM       PIC S9(3)V COMP-3.
           10  75RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  75RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  75RT-CFLIAL-CNPJ            PIC S9(5)V COMP-3.
           10  75RT-CCTRL-CNPJ-CPF         PIC S9(2)V COMP-3.
           10  75RT-HULT-ATULZ             PIC X(26).
           10  75RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  75RT-CTERM                  PIC X(8).

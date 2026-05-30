      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  05-ARGUMENTOS-ENTRADA.

       05  05-COLUNAS-DA-TABELA.
           10  05-CCONTR-LIM-DESC          PIC S9(9)V COMP-3.
           10  05-CVRSAO-CONTR-LIM         PIC S9(3)V COMP-3.
           10  05-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  05-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  05-HULT-ATULZ               PIC X(26).
           10  05-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  05-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  05-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  05-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  05-RETORNO.
       05  05-COD-RETORNO                         PIC 9(04).
       05  05-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  05-PROXIMO-RESTART                     PIC 9(05).
       05  05-QTDE-RETORNADA                      PIC 9(03).
       05  05-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  05-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T05.
           10  05RT-CCONTR-LIM-DESC        PIC S9(9)V COMP-3.
           10  05RT-CVRSAO-CONTR-LIM       PIC S9(3)V COMP-3.
           10  05RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  05RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  05RT-HULT-ATULZ             PIC X(26).
           10  05RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  05RT-CTERM                  PIC X(8).

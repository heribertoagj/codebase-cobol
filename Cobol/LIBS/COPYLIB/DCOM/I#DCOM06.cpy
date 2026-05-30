      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  06-ARGUMENTOS-ENTRADA.

       05  06-COLUNAS-DA-TABELA.
           10  06-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  06-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
           10  06-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  06-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  06-HULT-ATULZ               PIC X(26).
           10  06-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  06-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  06-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  06-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  06-RETORNO.
       05  06-COD-RETORNO                         PIC 9(04).
       05  06-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  06-PROXIMO-RESTART                     PIC 9(05).
       05  06-QTDE-RETORNADA                      PIC 9(03).
       05  06-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  06-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T06.
           10  06RT-CCONVE-CLI-DESC        PIC S9(9)V COMP-3.
           10  06RT-CVRSAO-CONVE-CLI       PIC S9(3)V COMP-3.
           10  06RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  06RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  06RT-HULT-ATULZ             PIC X(26).
           10  06RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  06RT-CTERM                  PIC X(8).

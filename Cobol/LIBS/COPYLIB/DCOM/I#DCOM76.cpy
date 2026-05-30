      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  76-ARGUMENTOS-ENTRADA.

       05  76-COLUNAS-DA-TABELA.
           10  76-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  76-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
           10  76-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  76-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  76-CFLIAL-CNPJ              PIC S9(5)V COMP-3.
           10  76-CCTRL-CNPJ-CPF           PIC S9(2)V COMP-3.
           10  76-HULT-ATULZ               PIC X(26).
           10  76-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  76-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  76-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  76-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  76-RETORNO.
       05  76-COD-RETORNO                         PIC 9(04).
       05  76-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  76-PROXIMO-RESTART                     PIC 9(05).
       05  76-QTDE-RETORNADA                      PIC 9(03).
       05  76-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  76-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T76.
           10  76RT-CCONVE-CLI-DESC        PIC S9(9)V COMP-3.
           10  76RT-CVRSAO-CONVE-CLI       PIC S9(3)V COMP-3.
           10  76RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  76RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  76RT-CFLIAL-CNPJ            PIC S9(5)V COMP-3.
           10  76RT-CCTRL-CNPJ-CPF         PIC S9(2)V COMP-3.
           10  76RT-HULT-ATULZ             PIC X(26).
           10  76RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  76RT-CTERM                  PIC X(8).

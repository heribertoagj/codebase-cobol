      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB097 - DB2PRD.TCONVE_CTRL_FORNC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  97-ARGUMENTOS-ENTRADA.

       05  97-COLUNAS-DA-TABELA.
           10  97-CCONVE-CLI-DESC              PIC S9(9)V COMP-3.
           10  97-CVRSAO-CONVE-CLI             PIC S9(3)V COMP-3.
           10  97-CCNPJ-CPF-FORNC              PIC S9(9)V COMP-3.
           10  97-CFLIAL-CNPJ-FORNC            PIC S9(5)V COMP-3.
           10  97-DVCTO-PGTO-FORNC             PIC X(10).
           10  97-CCTRL-CNPJ-FORNC             PIC S9(2)V COMP-3.
           10  97-VUTLZD-VCTO-FORNC            PIC S9(15)V9(2) COMP-3.
           10  97-HULT-ATULZ                   PIC X(26).
           10  97-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  97-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  97-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  97-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  97-RETORNO.
       05  97-COD-RETORNO                         PIC 9(04).
       05  97-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  97-PROXIMO-RESTART                     PIC 9(05).
       05  97-QTDE-RETORNADA                      PIC 9(03).
       05  97-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  97-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T97.
           10  97RT-CCONVE-CLI-DESC            PIC S9(9)V COMP-3.
           10  97RT-CVRSAO-CONVE-CLI           PIC S9(3)V COMP-3.
           10  97RT-CCNPJ-CPF-FORNC            PIC S9(9)V COMP-3.
           10  97RT-CFLIAL-CNPJ-FORNC          PIC S9(5)V COMP-3.
           10  97RT-DVCTO-PGTO-FORNC           PIC X(10).
           10  97RT-CCTRL-CNPJ-FORNC           PIC S9(2)V COMP-3.
           10  97RT-VUTLZD-VCTO-FORNC          PIC S9(15)V9(2) COMP-3.
           10  97RT-HULT-ATULZ                 PIC X(26).
           10  97RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  97RT-CTERM                      PIC X(8).

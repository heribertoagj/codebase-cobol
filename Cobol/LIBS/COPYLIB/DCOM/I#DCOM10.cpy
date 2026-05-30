      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB010 - DB2PRD.TCOMIS_CONVE_CLI       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  10-ARGUMENTOS-ENTRADA.

       05  10-COLUNAS-DA-TABELA.
           10  10-CCONVE-CLI-DESC              PIC S9(9)V COMP-3.
           10  10-CVRSAO-CONVE-CLI             PIC S9(3)V COMP-3.
           10  10-DPGTO-COMIS-CONVE            PIC X(10).
           10  10-VPGTO-COMIS-CONVE            PIC S9(15)V9(2) COMP-3.
           10  10-CINDCD-RELAT-CONVN           PIC X(1).
           10  10-HULT-ATULZ                   PIC X(26).
           10  10-CTRANS-PROG-ATULZ            PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  10-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  10-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  10-RETORNO.
       05  10-COD-RETORNO                         PIC 9(04).
       05  10-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  10-PROXIMO-RESTART                     PIC 9(05).
       05  10-QTDE-RETORNADA                      PIC 9(03).
       05  10-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  10-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T10.
           10  10RT-CCONVE-CLI-DESC            PIC S9(9)V COMP-3.
           10  10RT-CVRSAO-CONVE-CLI           PIC S9(3)V COMP-3.
           10  10RT-DPGTO-COMIS-CONVE          PIC X(10).
           10  10RT-VPGTO-COMIS-CONVE          PIC S9(15)V9(2) COMP-3.
           10  10RT-CINDCD-RELAT-CONVN         PIC X(1).
           10  10RT-HULT-ATULZ                 PIC X(26).
           10  10RT-CTRANS-PROG-ATULZ          PIC X(8).

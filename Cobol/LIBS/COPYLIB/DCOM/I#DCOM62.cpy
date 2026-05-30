      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  62-ARGUMENTOS-ENTRADA.

       05  62-COLUNAS-DA-TABELA.
           10  62-CTPO-ISENC-DESC          PIC S9(3)V COMP-3.
           10  62-CPRODT                   PIC S9(3)V COMP-3.
           10  62-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  62-DINIC-VGCIA-ISENC        PIC X(10).
           10  62-DFIM-VGCIA-ISENC         PIC X(10).
           10  62-CINDCD-REST-VLR          PIC X(1).
           10  62-VREST-MAX-OPER           PIC S9(15)V9(2) COMP-3.
           10  62-HULT-ATULZ               PIC X(26).
           10  62-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  62-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  62-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  62-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  62-RETORNO.
       05  62-COD-RETORNO                         PIC 9(04).
       05  62-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  62-PROXIMO-RESTART                     PIC 9(05).
       05  62-QTDE-RETORNADA                      PIC 9(03).
       05  62-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  62-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T62.
           10  62RT-CTPO-ISENC-DESC        PIC S9(3)V COMP-3.
           10  62RT-CPRODT                 PIC S9(3)V COMP-3.
           10  62RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  62RT-DINIC-VGCIA-ISENC      PIC X(10).
           10  62RT-DFIM-VGCIA-ISENC       PIC X(10).
           10  62RT-CINDCD-REST-VLR        PIC X(1).
           10  62RT-VREST-MAX-OPER         PIC S9(15)V9(2) COMP-3.
           10  62RT-HULT-ATULZ             PIC X(26).
           10  62RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  62RT-CTERM                  PIC X(8).

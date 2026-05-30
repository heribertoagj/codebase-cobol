      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C6 - DB2PRD.TOPER_ISENC_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C6-ARGUMENTOS-ENTRADA.

       05  C6-COLUNAS-DA-TABELA.
           10  C6-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C6-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C6-CTPO-ISENC-DESC              PIC S9(3)V COMP-3.
           10  C6-HULT-ATULZ                   PIC X(26).
           10  C6-CABRGC-ISENC-DESC            PIC S9(3)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C6-RETORNO.
       05  C6-COD-RETORNO                         PIC 9(04).
       05  C6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C6-PROXIMO-RESTART                     PIC 9(05).
       05  C6-QTDE-RETORNADA                      PIC 9(03).
       05  C6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC6.
           10  C6RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C6RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C6RT-CTPO-ISENC-DESC            PIC S9(3)V COMP-3.
           10  C6RT-HULT-ATULZ                 PIC X(26).
           10  C6RT-CABRGC-ISENC-DESC          PIC S9(3)V COMP-3.

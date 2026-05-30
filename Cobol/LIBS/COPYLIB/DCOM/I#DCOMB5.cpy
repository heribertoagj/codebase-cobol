      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B5 - DB2PRD.THIST_OPER_AUTRZ       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B5-ARGUMENTOS-ENTRADA.

       05  B5-COLUNAS-DA-TABELA.
           10  B5-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B5-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
  LT       10  B5-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  B5-CTPO-AUTRZ-ESPCL             PIC S9(3)V COMP-3.
           10  B5-HULT-ATULZ                   PIC X(26).
           10  B5-PMIN-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  B5-PMAX-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  B5-PTX-JURO-OPER-DESC           PIC S9(3)V9(5) COMP-3.
           10  B5-CINDCD-FREQ-TX               PIC X(2).
           10  B5-CCTRO-CUSTO                  PIC X(4).
           10  B5-CAUTRZ-TX-TESOU              PIC S9(9)V COMP-3.
           10  B5-IAUTRZ-TX-TESOU              PIC X(40).
           10  B5-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B5-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B5-RETORNO.
       05  B5-COD-RETORNO                         PIC 9(04).
       05  B5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B5-PROXIMO-RESTART                     PIC 9(05).
       05  B5-QTDE-RETORNADA                      PIC 9(03).
       05  B5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB5.
           10  B5RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B5RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
  LT       10  B5RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  B5RT-CTPO-AUTRZ-ESPCL           PIC S9(3)V COMP-3.
           10  B5RT-HULT-ATULZ                 PIC X(26).
           10  B5RT-PMIN-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  B5RT-PMAX-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  B5RT-PTX-JURO-OPER-DESC         PIC S9(3)V9(5) COMP-3.
           10  B5RT-CINDCD-FREQ-TX             PIC X(2).
           10  B5RT-CCTRO-CUSTO                PIC X(4).
           10  B5RT-CAUTRZ-TX-TESOU            PIC S9(9)V COMP-3.
           10  B5RT-IAUTRZ-TX-TESOU            PIC X(40).
           10  B5RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B5RT-CTERM                      PIC X(8).

      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H9 - DB2PRD.TTRASI_OPER_AUTRZ      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  H9-ARGUMENTOS-ENTRADA.

       05  H9-COLUNAS-DA-TABELA.
           10  H9-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  H9-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
  LT       10  H9-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  H9-CTPO-AUTRZ-ESPCL             PIC S9(3)V COMP-3.
           10  H9-HULT-ATULZ                   PIC X(26).
           10  H9-PMIN-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  H9-PMAX-TX-JURO-REGRA           PIC S9(3)V9(3) COMP-3.
           10  H9-PTX-JURO-OPER-DESC           PIC S9(3)V9(5) COMP-3.
           10  H9-CINDCD-FREQ-TX               PIC X(2).
           10  H9-CCTRO-CUSTO                  PIC X(4).
           10  H9-CAUTRZ-TX-TESOU              PIC S9(9)V COMP-3.
           10  H9-IAUTRZ-TX-TESOU              PIC X(40).
           10  H9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H9-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H9-RETORNO.
       05  H9-COD-RETORNO                         PIC 9(04).
       05  H9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H9-PROXIMO-RESTART                     PIC 9(05).
       05  H9-QTDE-RETORNADA                      PIC 9(03).
       05  H9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH9.
           10  H9RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  H9RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
  LT       10  H9RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  H9RT-CTPO-AUTRZ-ESPCL           PIC S9(3)V COMP-3.
           10  H9RT-HULT-ATULZ                 PIC X(26).
           10  H9RT-PMIN-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  H9RT-PMAX-TX-JURO-REGRA         PIC S9(3)V9(3) COMP-3.
           10  H9RT-PTX-JURO-OPER-DESC         PIC S9(3)V9(5) COMP-3.
           10  H9RT-CINDCD-FREQ-TX             PIC X(2).
           10  H9RT-CCTRO-CUSTO                PIC X(4).
           10  H9RT-CAUTRZ-TX-TESOU            PIC S9(9)V COMP-3.
           10  H9RT-IAUTRZ-TX-TESOU            PIC X(40).
           10  H9RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H9RT-CTERM                      PIC X(8).

      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D5 - DB2PRD.TRELAC_CHEQ_IMPRE      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  D5-ARGUMENTOS-ENTRADA.

       05  D5-COLUNAS-DA-TABELA.
           10  D5-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  D5-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  D5-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  D5-CBCO                         PIC S9(3)V COMP-3.
           10  D5-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  D5-CCTA-CORR-BCRIA              PIC S9(11)V COMP-3.
           10  D5-CCHEQ                        PIC S9(7)V COMP-3.
           10  D5-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  D5-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  D5-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  D5-DVCTO-CHEQ-CTDIA             PIC X(10).
           10  D5-VCHEQC                       PIC S9(13)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D5-RETORNO.
       05  D5-COD-RETORNO                         PIC 9(04).
       05  D5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D5-PROXIMO-RESTART                     PIC 9(05).
       05  D5-QTDE-RETORNADA                      PIC 9(03).
       05  D5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TD5.
           10  D5RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  D5RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  D5RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  D5RT-CBCO                       PIC S9(3)V COMP-3.
           10  D5RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  D5RT-CCTA-CORR-BCRIA            PIC S9(11)V COMP-3.
           10  D5RT-CCHEQ                      PIC S9(7)V COMP-3.
           10  D5RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  D5RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  D5RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  D5RT-DVCTO-CHEQ-CTDIA           PIC X(10).
           10  D5RT-VCHEQC                     PIC S9(13)V9(2) COMP-3.

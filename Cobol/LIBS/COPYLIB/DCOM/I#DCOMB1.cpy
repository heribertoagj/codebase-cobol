      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B1 - DB2PRD.THIST_FORNC_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B1-ARGUMENTOS-ENTRADA.

       05  B1-COLUNAS-DA-TABELA.
           10  B1-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B1-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B1-NFORNC-DESC-COML             PIC S9(4)V COMP-3.
           10  B1-HULT-ATULZ                   PIC X(26).
           10  B1-CDOCTO-FSCAL-DESC            PIC X(20).
           10  B1-CSERIE-DOCTO-FSCAL           PIC X(5).
           10  B1-DEMIS-DOCTO-FSCAL            PIC X(10).
           10  B1-DVCTO-DOCTO-FSCAL            PIC X(10).
           10  B1-VDOCTO-FSCAL                 PIC S9(15)V9(2) COMP-3.
           10  B1-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  B1-CCONDC-PGTO-FORNC            PIC S9(5)V COMP-3.
           10  B1-CIDTFD-SEQ-PGTO              PIC X(16).
           10  B1-CORIGE-PGTO-FORNC            PIC S9(1)V COMP-3.
           10  B1-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  B1-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  B1-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  B1-CINDCD-CONF-UTILZ            PIC X(1).
           10  B1-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  B1-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  B1-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  B1-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  B1-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  B1-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B1-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B1-RETORNO.
       05  B1-COD-RETORNO                         PIC 9(04).
       05  B1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B1-PROXIMO-RESTART                     PIC 9(05).
       05  B1-QTDE-RETORNADA                      PIC 9(03).
       05  B1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB1.
           10  B1RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B1RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B1RT-NFORNC-DESC-COML           PIC S9(4)V COMP-3.
           10  B1RT-HULT-ATULZ                 PIC X(26).
           10  B1RT-CDOCTO-FSCAL-DESC          PIC X(20).
           10  B1RT-CSERIE-DOCTO-FSCAL         PIC X(5).
           10  B1RT-DEMIS-DOCTO-FSCAL          PIC X(10).
           10  B1RT-DVCTO-DOCTO-FSCAL          PIC X(10).
           10  B1RT-VDOCTO-FSCAL               PIC S9(15)V9(2) COMP-3.
           10  B1RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  B1RT-CCONDC-PGTO-FORNC          PIC S9(5)V COMP-3.
           10  B1RT-CIDTFD-SEQ-PGTO            PIC X(16).
           10  B1RT-CORIGE-PGTO-FORNC          PIC S9(1)V COMP-3.
           10  B1RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  B1RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  B1RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  B1RT-CINDCD-CONF-UTILZ          PIC X(1).
           10  B1RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  B1RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  B1RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  B1RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  B1RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  B1RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B1RT-CTERM                      PIC X(8).

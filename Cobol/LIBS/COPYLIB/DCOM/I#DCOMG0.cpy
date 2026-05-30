      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G0 - DB2PRD.TTRASI_FORNC_OPER      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G0-ARGUMENTOS-ENTRADA.

       05  G0-COLUNAS-DA-TABELA.
           10  G0-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G0-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G0-NFORNC-DESC-COML             PIC S9(4)V COMP-3.
           10  G0-CDOCTO-FSCAL-DESC            PIC X(20).
           10  G0-CSERIE-DOCTO-FSCAL           PIC X(5).
           10  G0-DEMIS-DOCTO-FSCAL            PIC X(10).
           10  G0-DVCTO-DOCTO-FSCAL            PIC X(10).
           10  G0-VDOCTO-FSCAL                 PIC S9(15)V9(2) COMP-3.
           10  G0-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  G0-CCONDC-PGTO-FORNC            PIC S9(5)V COMP-3.
           10  G0-CIDTFD-SEQ-PGTO              PIC X(16).
           10  G0-CORIGE-PGTO-FORNC            PIC S9(1)V COMP-3.
           10  G0-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  G0-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  G0-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  G0-CINDCD-CONF-UTILZ            PIC X(1).
           10  G0-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  G0-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  G0-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  G0-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  G0-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  G0-CINDCD-SELEC-DESC            PIC X(1).
           10  G0-HULT-ATULZ                   PIC X(26).
           10  G0-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G0-CTERM                        PIC X(8).
           10  G0-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G0-RETORNO.
       05  G0-COD-RETORNO                         PIC 9(04).
       05  G0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G0-PROXIMO-RESTART                     PIC 9(05).
       05  G0-QTDE-RETORNADA                      PIC 9(03).
       05  G0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG0.
           10  G0RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G0RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G0RT-NFORNC-DESC-COML           PIC S9(4)V COMP-3.
           10  G0RT-CDOCTO-FSCAL-DESC          PIC X(20).
           10  G0RT-CSERIE-DOCTO-FSCAL         PIC X(5).
           10  G0RT-DEMIS-DOCTO-FSCAL          PIC X(10).
           10  G0RT-DVCTO-DOCTO-FSCAL          PIC X(10).
           10  G0RT-VDOCTO-FSCAL               PIC S9(15)V9(2) COMP-3.
           10  G0RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  G0RT-CCONDC-PGTO-FORNC          PIC S9(5)V COMP-3.
           10  G0RT-CIDTFD-SEQ-PGTO            PIC X(16).
           10  G0RT-CORIGE-PGTO-FORNC          PIC S9(1)V COMP-3.
           10  G0RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  G0RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  G0RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  G0RT-CINDCD-CONF-UTILZ          PIC X(1).
           10  G0RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  G0RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  G0RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  G0RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  G0RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  G0RT-CINDCD-SELEC-DESC          PIC X(1).
           10  G0RT-HULT-ATULZ                 PIC X(26).
           10  G0RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G0RT-CTERM                      PIC X(8).
           10  G0RT-DINCL-REG                  PIC X(10).

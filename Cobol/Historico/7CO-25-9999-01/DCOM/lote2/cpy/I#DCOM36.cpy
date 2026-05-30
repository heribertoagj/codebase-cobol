      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB036 - DB2PRD.TOPER_ISENC_PSSOA      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  36-ARGUMENTOS-ENTRADA.

       05  36-COLUNAS-DA-TABELA.
           10  36-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  36-NSEQ-PSSOA-DESC              PIC S9(3)V COMP-3.
           10  36-CTPO-ISENC-DESC              PIC S9(3)V COMP-3.
           10  36-DINIC-VGCIA-ISENC            PIC X(10).
           10  36-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  36-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  36-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  36-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  36-CCLUB                        PIC S9(10)V COMP-3.
           10  36-CINDCD-PSSOA-DESC            PIC X(2).
           10  36-DFIM-VGCIA-ISENC             PIC X(10).
           10  36-HULT-ATULZ                   PIC X(26).
           10  36-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  36-CTERM                        PIC X(8).
           10  36-CBCO                         PIC S9(3)V COMP-3.
           10  36-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  36-CCTA-BCRIA-CLI               PIC S9(13)V USAGE COMP-3.
4S2511     10  36-CCNPJ-CPF-ST                 PIC X(9).
4S2511     10  36-CFLIAL-CNPJ-ST               PIC X(4).
4S2511     10  36-CCTRL-CNPJ-CPF-ST            PIC S9(2)V USAGE COMP-3.          


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  36-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  36-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  36-RETORNO.
       05  36-COD-RETORNO                         PIC 9(04).
       05  36-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  36-PROXIMO-RESTART                     PIC 9(05).
       05  36-QTDE-RETORNADA                      PIC 9(03).
       05  36-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  36-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T36.
           10  36RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  36RT-NSEQ-PSSOA-DESC            PIC S9(3)V COMP-3.
           10  36RT-CTPO-ISENC-DESC            PIC S9(3)V COMP-3.
           10  36RT-DINIC-VGCIA-ISENC          PIC X(10).
           10  36RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  36RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  36RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  36RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  36RT-CCLUB                      PIC S9(10)V COMP-3.
           10  36RT-CINDCD-PSSOA-DESC          PIC X(2).
           10  36RT-DFIM-VGCIA-ISENC           PIC X(10).
           10  36RT-HULT-ATULZ                 PIC X(26).
           10  36RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  36RT-CTERM                      PIC X(8).
           10  36RT-CBCO                       PIC S9(3)V COMP-3.
           10  36RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  36RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
4S2511     10  36RT-CCNPJ-CPF-ST               PIC X(9).
4S2511     10  36RT-CFLIAL-CNPJ-ST             PIC X(4).
4S2511     10  36RT-CCTRL-CNPJ-CPF-ST          PIC S9(2)V USAGE COMP-3.          

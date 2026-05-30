      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  63-ARGUMENTOS-ENTRADA.

       05  63-COLUNAS-DA-TABELA.
           10  63-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  63-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  63-CTPO-ISENC-DESC          PIC S9(3)V COMP-3.
           10  63-DINIC-VGCIA-ISENC        PIC X(10).
           10  63-CPRODT                   PIC S9(3)V COMP-3.
           10  63-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  63-DFIM-VGCIA-ISENC         PIC X(10).
           10  63-CFLIAL-CNPJ              PIC S9(5)V COMP-3.
           10  63-CCTRL-CNPJ-CPF           PIC S9(2)V COMP-3.
           10  63-CCLUB                    PIC S9(10)V COMP-3.
           10  63-CINDCD-PSSOA-DESC        PIC X(2).
           10  63-CINDCD-REST-VLR          PIC X(1).
           10  63-VREST-MAX-OPER           PIC S9(15)V9(2) COMP-3.
           10  63-HULT-ATULZ               PIC X(26).
           10  63-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  63-CTERM                    PIC X(8).
4S2511     10  63-CCNPJ-CPF-ST             PIC X(09).   
4S2511     10  63-CFLIAL-CNPJ-ST           PIC X(04).
4S2511     10  63-CCTRL-CNPJ-CPF-ST        PIC S9(02)V COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  63-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  63-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  63-RETORNO.
       05  63-COD-RETORNO                         PIC 9(04).
       05  63-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  63-PROXIMO-RESTART                     PIC 9(05).
       05  63-QTDE-RETORNADA                      PIC 9(03).
       05  63-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  63-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T63.
           10  63RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  63RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  63RT-CTPO-ISENC-DESC        PIC S9(3)V COMP-3.
           10  63RT-DINIC-VGCIA-ISENC      PIC X(10).
           10  63RT-CPRODT                 PIC S9(3)V COMP-3.
           10  63RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  63RT-DFIM-VGCIA-ISENC       PIC X(10).
           10  63RT-CFLIAL-CNPJ            PIC S9(5)V COMP-3.
           10  63RT-CCTRL-CNPJ-CPF         PIC S9(2)V COMP-3.
           10  63RT-CCLUB                  PIC S9(10)V COMP-3.
           10  63RT-CINDCD-PSSOA-DESC      PIC X(2).
           10  63RT-CINDCD-REST-VLR        PIC X(1).
           10  63RT-VREST-MAX-OPER         PIC S9(15)V9(2) COMP-3.
           10  63RT-HULT-ATULZ             PIC X(26).
           10  63RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  63RT-CTERM                  PIC X(8).
4S2511     10  63RT-CCNPJ-CPF-ST             PIC X(09).   
4S2511     10  63RT-CFLIAL-CNPJ-ST           PIC X(04).
4S2511     10  63RT-CCTRL-CNPJ-CPF-ST        PIC S9(02)V COMP-3.

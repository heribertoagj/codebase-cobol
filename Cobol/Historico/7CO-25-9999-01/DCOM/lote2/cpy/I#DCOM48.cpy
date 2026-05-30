      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  48-ARGUMENTOS-ENTRADA.

       05  48-COLUNAS-DA-TABELA.
           10  48-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  48-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  48-DINIC-VGCIA-ISENC        PIC X(10).
           10  48-CTPO-ISENC-DESC          PIC S9(3)V COMP-3.
           10  48-DFIM-VGCIA-ISENC         PIC X(10).
           10  48-CFLIAL-CNPJ              PIC S9(5)V COMP-3.
           10  48-CCTRL-CNPJ-CPF           PIC S9(2)V COMP-3.
           10  48-CCLUB                    PIC S9(10)V COMP-3.
           10  48-CABRGC-ISENC-DESC        PIC S9(3)V COMP-3.
           10  48-CINDCD-PSSOA-DESC        PIC X(2).
           10  48-CINDCD-REST-CDENT        PIC X(1).
           10  48-HULT-ATULZ               PIC X(26).
           10  48-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  48-CTERM                    PIC X(8).
4S2511     10  48-CCNPJ-CPF-ST             PIC X(09).   
4S2511     10  48-CFLIAL-CNPJ-ST           PIC X(04).
4S2511     10  48-CCTRL-CNPJ-CPF-ST        PIC S9(02)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  48-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  48-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  48-RETORNO.
       05  48-COD-RETORNO                         PIC 9(04).
       05  48-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  48-PROXIMO-RESTART                     PIC 9(05).
       05  48-QTDE-RETORNADA                      PIC 9(03).
       05  48-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  48-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T48.
           10  48RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  48RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  48RT-DINIC-VGCIA-ISENC      PIC X(10).
           10  48RT-CTPO-ISENC-DESC        PIC S9(3)V COMP-3.
           10  48RT-DFIM-VGCIA-ISENC       PIC X(10).
           10  48RT-CFLIAL-CNPJ            PIC S9(5)V COMP-3.
           10  48RT-CCTRL-CNPJ-CPF         PIC S9(2)V COMP-3.
           10  48RT-CCLUB                  PIC S9(10)V COMP-3.
           10  48RT-CABRGC-ISENC-DESC      PIC S9(3)V COMP-3.
           10  48RT-CINDCD-PSSOA-DESC      PIC X(2).
           10  48RT-CINDCD-REST-CDENT      PIC X(1).
           10  48RT-HULT-ATULZ             PIC X(26).
           10  48RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  48RT-CTERM                  PIC X(8).
4S2511     10  48RT-CCNPJ-CPF-ST             PIC X(09).   
4S2511     10  48RT-CFLIAL-CNPJ-ST           PIC X(04).
4S2511     10  48RT-CCTRL-CNPJ-CPF-ST        PIC S9(02)V COMP-3.

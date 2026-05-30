      *----------------------------------------------------------------*
      *    INC = I#DCOMJ3                                              *
      *----------------------------------------------------------------*
      *        AREA DE COMUNICACAO DO MODULO DCOM5524                  *
      *        OBTER DADOS DOS CHEQUES DA CUSTODIA E/OU TABELA DE      *
      *        CHEQUE MODIFICACAO.                                     *
      *                                                                *
      ******************************************************************
       01  J3-ARGUMENTOS-ENTRADA.

       05  J3-COLUNAS-DA-TABELA.
           10  J3-DANO-OPER-DESC               PIC  9(4).
           10  J3-NSEQ-OPER-DESC               PIC  9(9).
           10  J3-NPCELA-DESC-COML             PIC  9(5).
           10  J3-TIPO-OPCAO                   PIC  X(1).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  J3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-----------------------------------------------*
       05  J3-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  J3-RETORNO.
       05  J3-COD-RETORNO                PIC 9(04).
       05  J3-MENSAGEM                   PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  J3-PROXIMO-RESTART            PIC 9(05).
       05  J3-QTDE-RETORNADA             PIC 9(03).
       05  J3-QTDE-TOTAL                 PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  J3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJ3.
           10  J3RT-BANCO                PIC  9(003).
           10  J3RT-AGENCIA              PIC  9(005).
           10  J3RT-CONTA                PIC  9(013).
           10  J3RT-NUMERO               PIC  9(008).
           10  J3RT-VALOR                PIC  9(015)V99.
           10  J3RT-SITUACAO             PIC  X(020).
           10  J3RT-NRO-CNPJ-CPF-CHEQ    PIC  9(009).
           10  J3RT-FLIAL-CPNJ-CPF-CHEQ  PIC  9(005).
           10  J3RT-CTRL-CNPJ-CPF-CHEQ   PIC  9(002).

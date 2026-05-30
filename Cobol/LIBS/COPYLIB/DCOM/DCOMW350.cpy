      ******************************************************************
      * NOME BOOK : DCOMW350                                           *
      * DESCRICAO : COMUNICACAO CICS X IMS (DCOM3500 X DCOM5524)       *
      * DATA      : JUN/2023                                           *
      * AUTOR     : LEVI BARCIELA DA SILVA CHAVES                      *
      * EMPRESA   : CAPGEMINI                                          *
      ******************************************************************
      *        AREA DE COMUNICACAO DO MODULO DCOM3500                  *
      *        COM O OBJETIVO DE SER ACIONADO POR COMUNICACAO          *
      *        CICS X IMS E CHAMAR O MODULO DCOM5524 PARA              *
      *        OBTER DADOS DOS CHEQUES DA CUSTODIA E/OU TABELA DE      *
      *        CHEQUE MODIFICACAO.                                     *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
           05 W350-DADOS.
              10   W350-AREA-ENTRADA.
                   15  W350-DANO-OPER-DESC             PIC  9(4).
                   15  W350-NSEQ-OPER-DESC             PIC  9(9).
                   15  W350-NPCELA-DESC-COML           PIC  9(5).
                   15  W350-TIPO-OPCAO                 PIC  X(1).
                   15  W350-QTDE-A-DESPREZAR           PIC  9(05).
                   15  W350-INSTRUCAO                  PIC  X(10).
                   15  W350-RESERVA                    PIC  X(35).
              10   W350-AREA-SAIDA.
                   15  W350-COD-RETORNO                PIC 9(04).
                   15  W350-MENSAGEM                   PIC X(79).
                   15  W350-LOCAL                      PIC X(04).
                   15  W350-QTDE-RETORNADA             PIC 9(03).

                   15  W350-OCORRENCIAS       OCCURS 010 TIMES.
                       20  W350-BANCO                    PIC  9(003).
                       20  W350-AGENCIA                  PIC  9(005).
                       20  W350-CONTA                    PIC  9(013).
                       20  W350-NUMERO                   PIC  9(008).
                       20  W350-VALOR                    PIC  9(015)V99.
                       20  W350-SITUACAO                 PIC  X(020).
                       20  W350-NRO-CNPJ-CPF-CHEQ        PIC  9(009).
                       20  W350-FLIAL-CPNJ-CPF-CHEQ      PIC  9(005).
                       20  W350-CTRL-CNPJ-CPF-CHEQ       PIC  9(002).
              10   W350-RESERVA                      PIC  X(0621).
           05 W350-RESERVA                           PIC  X(005).



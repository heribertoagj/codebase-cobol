      ******************************************************************
      * NOME BOOK : BVVEW007                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS PARA OPT-OUT            *
      * DATA      : 21/06/2019                                         *
      * AUTOR     : FLAVIO AUGUSTO MARIA                               *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 2754 BYTES                                         *
      ******************************************************************
       01  BVVEW007-AREA.
           05 BVVEW007-E-ENTRADA.
              10 BVVEW007-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW007-E-PAGINA                   PIC  9(003).
              10 BVVEW007-E-CPF-CNPJ.
                 15 BVVEW007-E-CCPF                  PIC  9(009).
                 15 BVVEW007-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW007-E-CPF-DIG               PIC  9(002).
              10 BVVEW007-E-AGENCIA                  PIC  9(005).
              10 BVVEW007-E-CONTA                    PIC  9(007).
              10 BVVEW007-E-DIG-CONTA                PIC  9(001).
           05 BVVEW007-S-SAIDA.
              10 BVVEW007-S-COD-RETORNO              PIC  9(005).
              10 BVVEW007-S-QTDE-OCORR               PIC  9(005).
              10 BVVEW007-S-FIM-DADOS                PIC  X(001).
              10 BVVEW007-S-MENSAGEM                 PIC  X(079).
              10 BVVEW007-S-DADOS OCCURS 24 TIMES.
                 15 BVVEW007-S-CCRDRA-ESTBL-COML     PIC  9(004).
                 15 BVVEW007-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                 15 BVVEW007-S-CBANDE-VISAO          PIC  9(003).
                 15 BVVEW007-S-RBANDE-VISAO          PIC  X(060).
           05 BVVEW007-S-RESERVA.
              10 FILLER                              PIC  X(300).

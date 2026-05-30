      ******************************************************************
      * NOME BOOK : BVVEW013                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS POR AGENCIA/CONTA       *
      * DATA      : 06/07/2019                                         *
      * AUTOR     : WALTER                                             *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 1919 BYTES                                         *
      ******************************************************************
       01  BVVEW013-AREA.
           05 BVVEW013-E-ENTRADA.
              10 BVVEW013-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW013-E-PAGINA                   PIC  9(003).
              10 BVVEW013-E-AGENCIA                  PIC  9(005).
              10 BVVEW013-E-CONTA                    PIC  9(013).
              10 BVVEW013-E-DIG                      PIC  9(001).
              10 BVVEW013-E-DOMICILIADO              PIC  X(001).
           05 BVVEW013-S-SAIDA.
              10 BVVEW013-S-COD-RETORNO              PIC  9(005).
              10 BVVEW013-S-QTDE-OCORR               PIC  9(005).
              10 BVVEW013-S-FIM-DADOS                PIC  X(001).
              10 BVVEW013-S-DADOS OCCURS 13 TIMES.
                 15 BVVEW013-S-CPRODT-AGNDA-FINCR    PIC  9(005).
                 15 BVVEW013-S-CCRDRA-ESTBL-COML     PIC  9(004).
                 15 BVVEW013-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                 15 BVVEW013-S-CBANDE-VISAO          PIC  9(003).
                 15 BVVEW013-S-RBANDE-VISAO          PIC  X(060).
                 15 BVVEW013-S-CPF-CNPJ.
                    20 BVVEW013-S-CCPF               PIC  9(009).
                    20 BVVEW013-S-CNPJ-FILIAL        PIC  9(004).
                    20 BVVEW013-S-CPF-CONTR          PIC  9(002).
                 15 BVVEW013-S-NUM-CIP               PIC  9(011).
              10 BVVEW013-S-MENSAGEM                 PIC  X(079).
          03 BVVEW013-A-DADOS-APOIO.
             05 BVVEW013-A-FIM-DADOS             PIC X(001).
             05 BVVEW013-A-SQLCA                 PIC X(136).

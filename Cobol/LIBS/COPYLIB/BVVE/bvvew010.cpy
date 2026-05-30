      ******************************************************************
      * NOME BOOK : BVVEW010                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS                         *
      * DATA      : 04/06/2019                                         *
      * AUTOR     : VITOR HUGO DE FREITAS SANTOS                       *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 1528 BYTES                                         *
      ******************************************************************
       01  BVVEW010-AREA.
           05 BVVEW010-E-ENTRADA.
              10 BVVEW010-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW010-E-PAGINA                   PIC  9(003).
              10 BVVEW010-E-CPF-CNPJ.
                 15 BVVEW010-E-CCPF                  PIC  9(009).
                 15 BVVEW010-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW010-E-CPF-DIG               PIC  9(002).
              10 BVVEW010-E-AGENCIA                  PIC  9(005).
              10 BVVEW010-E-CONTA                    PIC  9(013).
           05 BVVEW010-S-SAIDA.
              10 BVVEW010-S-COD-RETORNO              PIC  9(005).
              10 BVVEW010-S-QTDE-OCORR               PIC  9(005).
              10 BVVEW010-S-FIM-DADOS                PIC  X(001).
              10 BVVEW010-S-DADOS OCCURS 12 TIMES.
                 15 BVVEW010-S-CPRODT-AGNDA-FINCR    PIC  9(005).
                 15 BVVEW010-S-CCRDRA-ESTBL-COML     PIC  9(004).
                 15 BVVEW010-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                 15 BVVEW010-S-CBANDE-VISAO          PIC  9(003).
                 15 BVVEW010-S-RBANDE-VISAO          PIC  X(060).
                 15 BVVEW010-S-DOMICILIADO           PIC  X(001).
                 15 BVVEW010-S-SOLICITACAO           PIC  X(001).
                 15 BVVEW010-S-CSIT-CADTO-SOLTC      PIC  9(001).
              10 BVVEW010-S-MENSAGEM                 PIC  X(079).
          03 BVVEW010-A-DADOS-APOIO.
             05 BVVEW010-A-FIM-DADOS             PIC X(001).
             05 BVVEW010-A-SQLCA                 PIC X(136).

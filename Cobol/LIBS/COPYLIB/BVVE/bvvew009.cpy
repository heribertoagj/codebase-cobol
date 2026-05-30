      ******************************************************************
      * NOME BOOK : BVVEW009                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS                         *
      * DATA      : 04/06/2019                                         *
      * AUTOR     : VITOR HUGO DE FREITAS SANTOS                       *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 2429 BYTES                                         *
      ******************************************************************
       01  BVVEW009-AREA.
           05 BVVEW009-E-ENTRADA.
              10 BVVEW009-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW009-E-PAGINA                   PIC  9(003).
              10 BVVEW009-E-CPF-CNPJ.
                 15 BVVEW009-E-CCPF                  PIC  9(009).
                 15 BVVEW009-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW009-E-CPF-DIG               PIC  9(002).
              10 BVVEW009-E-AGENCIA                  PIC  9(005).
              10 BVVEW009-E-CONTA                    PIC  9(013).
              10 BVVEW009-E-DOMICILIADO              PIC  X(001).

           05 BVVEW009-S-SAIDA.
              10 BVVEW009-S-COD-RETORNO              PIC  9(005).
              10 BVVEW009-S-QTDE-OCORR               PIC  9(005).
              10 BVVEW009-S-FIM-DADOS                PIC  X(001).
              10 BVVEW009-S-DADOS OCCURS 12 TIMES.
                 15 BVVEW009-S-CPRODT-AGNDA-FINCR    PIC  9(005).
                 15 BVVEW009-S-CCRDRA-ESTBL-COML     PIC  9(004).
                 15 BVVEW009-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                 15 BVVEW009-S-CBANDE-VISAO          PIC  9(003).
                 15 BVVEW009-S-RBANDE-VISAO          PIC  X(060).
                 15 BVVEW009-S-AGENCIA               PIC  9(005).
                 15 BVVEW009-S-CONTA                 PIC  9(013).
                 15 BVVEW009-S-DT-INIC               PIC  X(010).
                 15 BVVEW009-S-DT-FIM                PIC  X(010).
                 15 BVVEW009-S-COD-CIP               PIC  X(019).
                 15 BVVEW009-S-COD-DOMICILIADO       PIC  X(001).
                 15 BVVEW009-S-CIP                   PIC  X(020).

              10 BVVEW009-S-MENSAGEM                 PIC  X(079).
          03 BVVEW009-A-DADOS-APOIO.
             05 BVVEW009-A-FIM-DADOS             PIC X(001).
             05 BVVEW009-A-SQLCA                 PIC X(136).

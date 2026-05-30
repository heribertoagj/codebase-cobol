      *                                                                *
      ******************************************************************
      *                                                                *
      * NOME BOOK : BVVEW014                                           *
      *                                                                *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS                         *
      *                                                                *
      * DATA      : 08/07/2019                                         *
      *                                                                *
      * AUTOR     : REGINALDO PEDROSO DE AQUINO                        *
      *                                                                *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 1511 BYTES                                         *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVEW014-AREA.
           05 BVVEW014-E-ENTRADA.
              10 BVVEW014-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW014-E-PAGINA                   PIC  9(003).
              10 BVVEW014-E-CPF-CNPJ.
                 15 BVVEW014-E-CCPF                  PIC  9(009).
                 15 BVVEW014-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW014-E-CPF-DIG               PIC  9(002).
              10 BVVEW014-E-AGENCIA                  PIC  9(005).
              10 BVVEW014-E-CONTA                    PIC  9(013).
           05 BVVEW014-S-SAIDA.
              10 BVVEW014-S-COD-RETORNO              PIC  9(005).
              10 BVVEW014-S-QTDE-OCORR               PIC  9(005).
              10 BVVEW014-S-FIM-DADOS                PIC  X(001).
              10 BVVEW014-S-DADOS OCCURS 11 TIMES.
                 15 BVVEW014-S-NCADTO-AUTRZ-AGNDA    PIC  9(013).
                 15 BVVEW014-S-CCRDRA-ESTBL-COML     PIC  9(004).
                 15 BVVEW014-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                 15 BVVEW014-S-CBANDE-VISAO          PIC  9(003).
                 15 BVVEW014-S-RBANDE-VISAO          PIC  X(060).
                 15 BVVEW014-S-DOMICILIADO           PIC  X(001).
                 15 BVVEW014-S-SOLICITACAO           PIC  X(001).
                 15 BVVEW014-S-CSIT-CADTO-SOLTC      PIC  9(001).
              10 BVVEW014-S-MENSAGEM                 PIC  X(079).
      *                                                                *
          03 BVVEW014-A-DADOS-APOIO.
             05 BVVEW014-A-FIM-DADOS             PIC X(001).
             05 BVVEW014-A-SQLCA                 PIC X(136).
      *                                                                *

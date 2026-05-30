      ******************************************************************
      * NOME BOOK : BVVEW005                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA OBTER LISTA DE  *
      *             CREDENCIADORAS E BANDEIRAS PARA OPT-IN             *
      * DATA      : 04/06/2019                                         *
      * AUTOR     : FLAVIO AUGUSTO MARIA                               *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 2741 BYTES                                         *
      ******************************************************************
       01  BVVEW005-AREA.
           05 BVVEW005-E-ENTRADA.
              10 BVVEW005-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW005-E-PAGINA                   PIC  9(003).
              10 BVVEW005-E-CPF-CNPJ.
                 15 BVVEW005-E-CCPF                  PIC  9(009).
                 15 BVVEW005-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW005-E-CPF-DIG               PIC  9(002).
           05 BVVEW005-S-SAIDA.
              10 BVVEW005-S-COD-RETORNO              PIC  9(005).
              10 BVVEW005-S-QT-OCOR-CRE              PIC  9(005).
              10 BVVEW005-S-QT-OCOR-BAN              PIC  9(005).
              10 BVVEW005-S-FIM-DADO-BAND            PIC  X(001).
              10 BVVEW005-S-FIM-DADO-CRED            PIC  X(001).
              10 BVVEW005-S-MENSAGEM                 PIC  X(079).
              10 BVVEW005-S-CRED  OCCURS 22 TIMES.
                 15 BVVEW005-S-CCRDRA-ESTBL-COML     PIC  9(004).
                 15 BVVEW005-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
              10 BVVEW005-S-BAND  OCCURS 22 TIMES.
                 15 BVVEW005-S-CBANDE-VISAO          PIC  9(003).
                 15 BVVEW005-S-RBANDE-VISAO          PIC  X(060).
           05 BVVEW005-S-RESERVA.
              10 FILLER                              PIC  X(300).

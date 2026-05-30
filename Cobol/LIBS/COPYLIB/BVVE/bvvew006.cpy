      ******************************************************************
      * NOME BOOK : BVVEW006                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA AUTORIZACAO DE  *
      *             ENVIO DE AGENDA - OPT-IN                           *
      * DATA      : 10/06/2019                                         *
      * AUTOR     : FLAVIO AUGUSTO MARIA                               *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 618 BYTES                                          *
      * -------------------------------------------------------------- *
      * ESSE BOOK ESPERA RECEBER NO MAXIMO 22 REGISTROS PARA OPT-IN    *
      * POIS EH O NUMERO DE OCORRENCIAS QUE SAO EXIBIDAS NA TELA DO    *
      * PROGRAMA BVVE1002 DE AUTORIZACAO DE ENVIO DE AGENDA - OPT-IN.  *
      * -------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                   *
      * -------------------------------------------------------------- *
      * BVVEW006-E-SELECIONAR-TODAS: INDICA SE A OPCAO DA TELA 'MARCAR *
      *                              TODAS' FOI SELECIONADA. QUANDO    *
      *                              TIVER COM 'S' NAO PRECISA INFORMAR*
      *                              OS SEGUINTES CAMPOS DE ENTRADA:   *
      *                              - BVVEW006-E-QTDE-OCORR           *
      *                              - BVVEW006-E-CD-CREDENCI          *
      *                              - BVVEW006-E-CD-BANDEIRA          *
      *                              S - SIM                           *
      *                              N - NAO                           *
      * BVVEW006-E-QTDE-OCORR      : QUANTIDADE DE REGISTROS QUE FORAM *
      *                              SELECIONADOS NA PAGINA.           *
      *                              OBRIGATORIO QUANDO                *
      *                              SELECIONAR-TODAS IGUAL 'N'.       *
      * BVVEW006-E-USUARIO         : USUARIO LOGADO. OBRIGATORIO.      *
      * BVVEW006-E-CPF-CNPJ        : CPF/CNPJ DO CLIENTE QUE FOI INFOR-*
      *                              MADO NA TELA. OBRIGATORIO.        *
      * BVVEW006-E-AGENCIA         : AGENCIA DO CLIENTE QUE FOI INFOR- *
      *                              MADA NA TELA. OPCIONAL. SE NAO FOR*
      *                              INFORMADA MOVER ZERO.             *
      * BVVEW006-E-CONTA           : CONTA DO CLIENTE QUE FOI INFORMADA*
      *                              NA TELA. OPCIONAL. SE NAO FOR     *
      *                              INFORMADA MOVER ZERO.             *
      * BVVEW006-E-DIG-CONTA       : DIGITO DA CONTA DO CLIENTE QUE FOI*
      *                              INFORMADO NA TELA. OPCIONAL. SE   *
      *                              NAO FOR INFORMADO MOVER ZERO.     *
      * BVVEW006-E-CINDCD-DOMCL-BCRIO: CAMPO 'DOMICILIADO' DA TELA.    *
      *                              OPCIONAL. SE NAO FOR INFORMADO    *
      *                              MOVER ESPACO.                     *
      * BVVEW006-E-CD-CREDENCI     : CODIGO DA CREDENCIADORA QUE FOI   *
      *                              SELECIONADO NA TELA.              *
      *                              OBRIGATORIO QUANDO                *
      *                              SELECIONAR-TODAS IGUAL 'N'.       *
      * BVVEW006-E-CD-BANDEIRA     : CODIGO DA BANDEIRA QUE FOI        *
      *                              SELECIONADO NA TELA.              *
      *                              OBRIGATORIO QUANDO                *
      *                              SELECIONAR-TODAS IGUAL 'N'.       *
      ******************************************************************
       01  BVVEW006-AREA.
           05 BVVEW006-E-ENTRADA.
              10 BVVEW006-E-SEL-TODAS-CRE            PIC  X(001).
              10 BVVEW006-E-SEL-TODAS-BAN            PIC  X(001).
              10 BVVEW006-E-QT-OCORR-CRE             PIC  9(005).
              10 BVVEW006-E-QT-OCORR-BAN             PIC  9(005).
              10 BVVEW006-E-USUARIO                  PIC  X(009).
              10 BVVEW006-E-CPF-CNPJ.
                 15 BVVEW006-E-CCPF                  PIC  9(009).
                 15 BVVEW006-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW006-E-CPF-DIG               PIC  9(002).
              10 BVVEW006-E-AGENCIA                  PIC  9(005).
              10 BVVEW006-E-CONTA                    PIC  9(007).
              10 BVVEW006-E-DIG-CONTA                PIC  9(001).
              10 BVVEW006-E-CINDCD-DOMCL-BCRIO       PIC  X(001).
              10 BVVEW006-E-DATA-ASS                 PIC  X(010).
              10 BVVEW006-E-DATA-INI                 PIC  X(010).
              10 BVVEW006-E-DATA-FIM                 PIC  X(010).
              10 BVVEW006-E-SELECAO-OCORR.
                 11 FILLER                           OCCURS 22 TIMES.
                    15 BVVEW006-E-CD-CRED            PIC  9(04).
                    15 BVVEW006-E-CD-BAND            PIC  9(03).
           05 BVVEW006-S-SAIDA.
              10 BVVEW006-S-COD-RETORNO              PIC  9(005).
              10 BVVEW006-S-MENSAGEM                 PIC  X(079).
           05 BVVEW006-S-RESERVA.
              10 FILLER                              PIC  X(300).

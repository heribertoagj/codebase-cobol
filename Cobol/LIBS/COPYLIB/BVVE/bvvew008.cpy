      ******************************************************************
      * NOME BOOK : BVVEW008                                           *
      * DESCRICAO : AREA DE COMUNICACAO DO MODULO PARA CANCELAMENTO DE *
      *             ENVIO DE AGENDA - OPT-OUT                          *
      * DATA      : 21/06/2019                                         *
      * AUTOR     : FLAVIO AUGUSTO MARIA                               *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 596 BYTES                                          *
      * -------------------------------------------------------------- *
      * ESSE BOOK ESPERA RECEBER NO MAXIMO 24 REGISTROS PARA OPT-OUT   *
      * POIS EH O NUMERO DE OCORRENCIAS QUE SAO EXIBIDAS NA TELA DO    *
      * PROGRAMA BVVE1004 DE CANCELAMENTO DE ENVIO DE AGENDA - OPT-OUT.*
      * -------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                   *
      * -------------------------------------------------------------- *
      * BVVEW008-E-SELECIONAR-TODAS: INDICA SE A OPCAO DA TELA 'MARCAR *
      *                              TODAS' FOI SELECIONADA. QUANDO    *
      *                              TIVER COM 'S' NAO PRECISA INFORMAR*
      *                              OS SEGUINTES CAMPOS DE ENTRADA:   *
      *                              - BVVEW008-E-QTDE-OCORR           *
      *                              - BVVEW008-E-CD-CREDENCI          *
      *                              - BVVEW008-E-CD-BANDEIRA          *
      *                              S - SIM                           *
      *                              N - NAO                           *
      * BVVEW008-E-QTDE-OCORR      : QUANTIDADE DE REGISTROS QUE FORAM *
      *                              SELECIONADOS NA PAGINA.           *
      *                              OBRIGATORIO QUANDO                *
      *                              SELECIONAR-TODAS IGUAL 'N'.       *
      * BVVEW008-E-USUARIO         : USUARIO LOGADO. OBRIGATORIO.      *
      * BVVEW008-E-CPF-CNPJ        : CPF/CNPJ DO CLIENTE QUE FOI INFOR-*
      *                              MADO NA TELA. OBRIGATORIO.        *
      * BVVEW008-E-AGENCIA         : AGENCIA DO CLIENTE QUE FOI INFOR- *
      *                              MADA NA TELA. OPCIONAL. SE NAO FOR*
      *                              INFORMADA MOVER ZERO.             *
      * BVVEW008-E-CONTA           : CONTA DO CLIENTE QUE FOI INFORMADA*
      *                              NA TELA. OPCIONAL. SE NAO FOR     *
      *                              INFORMADA MOVER ZERO.             *
      * BVVEW008-E-DIG-CONTA       : DIGITO DA CONTA DO CLIENTE QUE FOI*
      *                              INFORMADO NA TELA. OPCIONAL. SE   *
      *                              NAO FOR INFORMADO MOVER ZERO.     *
      * BVVEW008-E-CINDCD-DOMCL-BCRIO: CAMPO 'DOMICILIADO' DA TELA.    *
      *                              SE NAO FOR INFORMADO MOVER 'N'.   *
      * BVVEW008-E-CD-CREDENCI     : CODIGO DA CREDENCIADORA QUE FOI   *
      *                              SELECIONADO NA TELA.              *
      *                              OBRIGATORIO QUANDO                *
      *                              SELECIONAR-TODAS IGUAL 'N'.       *
      * BVVEW008-E-CD-BANDEIRA     : CODIGO DA BANDEIRA QUE FOI        *
      *                              SELECIONADO NA TELA.              *
      *                              OBRIGATORIO QUANDO                *
      *                              SELECIONAR-TODAS IGUAL 'N'.       *
      ******************************************************************
       01  BVVEW008-AREA.
           05 BVVEW008-E-ENTRADA.
              10 BVVEW008-E-SELECIONAR-TODAS         PIC  X(001).
              10 BVVEW008-E-QTDE-OCORR               PIC  9(005).
              10 BVVEW008-E-USUARIO                  PIC  X(009).
              10 BVVEW008-E-CPF-CNPJ.
                 15 BVVEW008-E-CCPF                  PIC  9(009).
                 15 BVVEW008-E-CPF-FILIAL            PIC  9(004).
                 15 BVVEW008-E-CPF-DIG               PIC  9(002).
              10 BVVEW008-E-AGENCIA                  PIC  9(005).
              10 BVVEW008-E-CONTA                    PIC  9(007).
              10 BVVEW008-E-DIG-CONTA                PIC  9(001).
              10 BVVEW008-E-CINDCD-DOMCL-BCRIO       PIC  X(001).
              10 BVVEW008-E-SELECAO-OCORR            OCCURS 24 TIMES.
                 15 BVVEW008-E-CD-CREDENCI           PIC  9(04).
                 15 BVVEW008-E-CD-BANDEIRA           PIC  9(03).
           05 BVVEW008-S-SAIDA.
              10 BVVEW008-S-COD-RETORNO              PIC  9(005).
              10 BVVEW008-S-MENSAGEM                 PIC  X(079).
           05 BVVEW008-S-RESERVA.
              10 FILLER                              PIC  X(300).

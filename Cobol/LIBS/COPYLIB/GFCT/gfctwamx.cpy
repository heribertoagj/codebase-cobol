      ******************************************************************
      * NOME DA INC - GFCTWAMX                                         *
      * DESCRICAO   - ADESAO INDIVIDUAL - DETALHE DA PERMISSAO DE ADSAO*
      *                                                      GFCT0124  *
      *               COMUNICACAO WEB X IMS      - SAIDA               *
      * TAMANHO     - 1010                                             *
      * DATA        - 03.2010                                          *
      * RESPONSAVEL - MARCUS VINICIUS            - PROCWORK - GP 50.   *
      ******************************************************************
      *
       01  GFCTWAMX-SAIDA.
           03  WAMX-LL                            PIC S9(004) COMP.
           03  WAMX-ZZ                            PIC S9(004) COMP.
           03  WAMX-TRANSACAO                     PIC  X(009).
           03  WAMX-FUNCAO                        PIC  X(008).
           03  WAMX-FUNC-BDSCO                    PIC  X(007).
           03  WAMX-ERRO                          PIC  9(001).
           03  WAMX-COD-MSG-ERRO                  PIC  9(007).
           03  WAMX-COD-SQL-ERRO                  PIC  9(003).
           03  WAMX-DESC-MSG-ERRO                 PIC  X(079).
           03  WAMX-DADOS1.
               05  WAMX-COD-TARIFA         PIC  9(005).
               05  WAMX-DESCR-TARIFA       PIC  X(040).
               05  WAMX-COD-AGPTO          PIC  9(003).
               05  WAMX-DESCR-AGPTO        PIC  X(040).
               05  WAMX-SEQUENCIA          PIC  9(009).
ST2506*        05  WAMX-CPF-CNPJ           PIC  9(009).
ST2506*        05  WAMX-FILIAL             PIC  9(005).
ST2506         05  WAMX-CPF-CNPJ           PIC  X(009).
ST2506         05  WAMX-FILIAL             PIC  X(004).
               05  WAMX-CONTROLE           PIC  9(002).
               05  WAMX-AGENCIA            PIC  9(005).
               05  WAMX-DESC-AGE           PIC  X(040).
               05  WAMX-CONTA              PIC  9(013).
               05  WAMX-DESC-CLIE          PIC  X(040).
               05  WAMX-POSTO              PIC  9(005).
               05  WAMX-DESC-POSTO         PIC  X(040).
               05  WAMX-SEGMENTO           PIC  9(003).
               05  WAMX-DESC-SEGTO         PIC  X(040).
               05  WAMX-MUNICIPIO          PIC  9(007).
               05  WAMX-DESC-MUNIC         PIC  X(040).
               05  WAMX-UF                 PIC  X(002).
               05  WAMX-DESC-UF            PIC  X(040).
               05  WAMX-TP-ADESAO          PIC  X(001).
      **********************************************************
BI0412* TIPO-ADESAO: I - INDIVIDUAL  C - COMPULSORIA  A - AMBAS
      **********************************************************
               05  WAMX-SOLICITA-EXCECAO   PIC  9(001).
      **********************************************************
BI0412* SE TIPO-ADESAO = C - COMPULSORIA  OU  A - AMBAS
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
BI0412         05  WAMX-PRMSS-INDVD        PIC  9(001).
BI0412**********************************************************
BI0412* SE TIPO-ADESAO = I - INDIVIDUAL  OU  A - AMBAS
BI0412*   PODERA SER:  0 - AMBOS  OU  1 - TARIFA  OU  2 - LEGADO
BI0412**********************************************************
               05  WAMX-DATA-INICIO        PIC  X(010).
               05  WAMX-DATA-FIM           PIC  X(010).
               05  WAMX-GERA-CRED-CEL      PIC  9(001).
      **********************************************************
      * GERA CREDITO: 0 - NAO GERA   OU  1 - GERA CREDITO
      **********************************************************
               05  WAMX-COD-FUNC-INCL      PIC  X(007).
               05  WAMX-DESC-FUNC-INCL     PIC  X(040).
               05  WAMX-DATA-INCL          PIC  X(010).
               05  WAMX-HORA-INCL          PIC  X(008).
               05  WAMX-COD-FUNC-CANCEL    PIC  X(007).
               05  WAMX-DESC-FUNC-CANCEL   PIC  X(040).
               05  WAMX-DATA-CANCEL        PIC  X(010).
               05  WAMX-HORA-CANCEL        PIC  X(008).
               05  WAMX-FIM                PIC  X(001).
BI0810         05  WAMX-CPSSOA-TARIFA      PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
BI0412         05  WAMX-VISUAL-COMPULS     PIC  9(001).
BI0412*           0  - NAO VISUALIZA COMPULSORIA
BI0412*           1  - VISUALIZA COMPULSORIA
ST2506*        05  FILLER                  PIC  X(347).
ST2506         05  FILLER                  PIC  X(348).

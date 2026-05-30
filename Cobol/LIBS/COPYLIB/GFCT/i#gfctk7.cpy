      *****************************************************************
      * NOME DA INC - I#GFCTK7                                        *
      * DESCRICAO   - PERMISSAO PARA ADESAO - INCLUSAO                *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTK7-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      * ALTERACAO   - VINICIUS - SOLICITACAO DE LEGADO, TARIFA OU     *
      *                AMBOS PARA ADESAO INDIVIDUAL EM OUTUBRO/2007   *
      *****************************************************************
       01  GFCTK7-ENTRADA.
           03 GFCTK7-LL                            PIC S9(004) COMP.
           03 GFCTK7-ZZ                            PIC S9(004) COMP.
           03 GFCTK7-TRANSACAO                     PIC  X(009).
           03 GFCTK7-FUNCAO                        PIC  X(008).
           03 GFCTK7-DADOS-RST.
              05 GFCTK7-FUNC-BDSCO                 PIC  X(007).
              05 GFCTK7-TERMINAL                   PIC  X(008).
              05 GFCTK7-FIM                        PIC  X(001).
BI0412        05 GFCTK7-FILLER                     PIC  X(629).
           03 GFCTK7-FIXO.
              05 GFCTK7-CPACOTE                    PIC  9(005).
              05 GFCTK7-RPACOTE                    PIC  X(030).
              05 GFCTK7-CAGPTO                     PIC  9(003).
              05 GFCTK7-RAGPTO                     PIC  X(030).
              05 GFCTK7-TOTAL                      PIC  X(001).
              05 GFCTK7-CGC-CPF                    PIC  9(009).
              05 GFCTK7-FILIAL                     PIC  9(005).
              05 GFCTK7-CONTROLE                   PIC  X(002).
              05 GFCTK7-CAGENCIA                   PIC  9(005).
              05 GFCTK7-RAGENCIA                   PIC  X(030).
              05 GFCTK7-CCONTA                     PIC  9(013).
              05 GFCTK7-RCONTA                     PIC  X(050).
              05 GFCTK7-CPAB                       PIC  9(005).
              05 GFCTK7-RPAB                       PIC  X(030).
              05 GFCTK7-CSEGMENTO                  PIC  9(003).
              05 GFCTK7-RSEGMENTO                  PIC  X(030).
              05 GFCTK7-CMUNIC                     PIC  9(005).
              05 GFCTK7-RMUNIC                     PIC  X(030).
              05 GFCTK7-CUF                        PIC  X(002).
              05 GFCTK7-RUF                        PIC  X(030).
              05 GFCTK7-DATA-INICIO                PIC  X(010).
              05 GFCTK7-DATA-FIM                   PIC  X(010).
              05 GFCTK7-TIPO-ADESAO                PIC  X(001).
      **********************************************************
      * TIPO-ADESAO: I - INDIVIDUAL  C - COMPULSORIA  A - AMBAS
      **********************************************************
              05 GFCTK7-SOLICITA-EXCECAO           PIC  9(001).
      **********************************************************
      * SE TIPO-ADESAO = C - COMPULSORIA  OU  A - AMBAS
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
BI0412        05 GFCTK7-PRMSS-INDVD                PIC  9(001).
BI0412**********************************************************
BI0412* SE TIPO-ADESAO = I - INDIVIDUAL   OU  A - AMBAS
BI0412*   PODERA SER:  0 - AMBOS  OU  1 - TARIFA  OU  2 - LEGADO
BI0412**********************************************************
              05 GFCTK7-GERA-CRED-CEL              PIC  9(001).
      **********************************************************
      * GERA CREDITO: 0 - NAO GERA   OU  1 - GERA CREDITO
      **********************************************************
BI0810        05 GFCTK7-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
BI0412        05 GFCTK7-VISUAL-COMPULS             PIC  9(001).
BI0412*           0  - NAO VISUALIZA COMPULSORIA
BI0412*           1  - VISUALIZA COMPULSORIA

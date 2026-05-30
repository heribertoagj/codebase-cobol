      *****************************************************************
      * NOME DA INC - I#GFCTK8                                        *
      * DESCRICAO   - PERMISSAO PARA ADESAO - INCLUSAO                *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTK8-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      * ALTERACAO   - VINICIUS - SOLICITACAO DE LEGADO, TARIFA OU     *
      *                AMBOS PARA ADESAO INDIVIDUAL EM OUTUBRO/2007   *
      *****************************************************************
       01  GFCTK8-SAIDA.
           03 GFCTK8-LL                            PIC S9(004) COMP.
           03 GFCTK8-ZZ                            PIC S9(004) COMP.
           03 GFCTK8-TRANSACAO                     PIC  X(009).
           03 GFCTK8-FUNCAO                        PIC  X(008).
           03 GFCTK8-DADOS-RST.
              05 GFCTK8-FUNC-BDSCO                 PIC  X(007).
              05 GFCTK8-TERMINAL                   PIC  X(008).
              05 GFCTK8-FIM                        PIC  X(001).
           03 GFCTK8-ERRO                          PIC  9(001).
           03 GFCTK8-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTK8-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTK8-DESC-MSG-ERRO                 PIC  X(079).
BI0412     03 GFCTK8-FILLER                        PIC  X(539).
           03 GFCTK8-FIXO.
              05 GFCTK8-CPACOTE                    PIC  9(005).
              05 GFCTK8-RPACOTE                    PIC  X(030).
              05 GFCTK8-CAGPTO                     PIC  9(003).
              05 GFCTK8-RAGPTO                     PIC  X(030).
              05 GFCTK8-TOTAL                      PIC  X(001).
              05 GFCTK8-CGC-CPF                    PIC  9(009).
              05 GFCTK8-FILIAL                     PIC  9(005).
              05 GFCTK8-CONTROLE                   PIC  X(002).
              05 GFCTK8-CAGENCIA                   PIC  9(005).
              05 GFCTK8-RAGENCIA                   PIC  X(030).
              05 GFCTK8-CCONTA                     PIC  9(013).
              05 GFCTK8-RCONTA                     PIC  X(050).
              05 GFCTK8-CPAB                       PIC  9(005).
              05 GFCTK8-RPAB                       PIC  X(030).
              05 GFCTK8-CSEGMENTO                  PIC  9(003).
              05 GFCTK8-RSEGMENTO                  PIC  X(030).
              05 GFCTK8-CMUNIC                     PIC  9(005).
              05 GFCTK8-RMUNIC                     PIC  X(030).
              05 GFCTK8-CUF                        PIC  X(002).
              05 GFCTK8-RUF                        PIC  X(030).
              05 GFCTK8-DATA-INICIO                PIC  X(010).
              05 GFCTK8-DATA-FIM                   PIC  X(010).
              05 GFCTK8-TIPO-ADESAO                PIC  X(001).
      **********************************************************
      * TIPO-ADESAO: I - INDIVIDUAL  C - COMPULSORIA  A - AMBAS
      **********************************************************
              05 GFCTK8-SOLICITA-EXCECAO           PIC  9(001).
      **********************************************************
      * SE TIPO-ADESAO = C - COMPULSORIA  OU  A - AMBAS
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
BI0412        05 GFCTK8-PRMSS-INDVD                PIC  9(001).
BI0412**********************************************************
BI0412* SE TIPO-ADESAO = I - INDIVIDUAL   OU  A - AMBAS
BI0412*   PODERA SER:  0 - AMBOS  OU  1 - TARIFA  OU  2 - LEGADO
BI0412**********************************************************
              05 GFCTK8-GERA-CRED-CEL              PIC  9(001).
      **********************************************************
      * GERA CREDITO: 0 - NAO GERA   OU  1 - GERA CREDITO
      **********************************************************
BI0810        05 GFCTK8-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
BI0412        05 GFCTK8-VISUAL-COMPULS             PIC  9(001).
BI0412*           0  - NAO VISUALIZA COMPULSORIA
BI0412*           1  - VISUALIZA COMPULSORIA

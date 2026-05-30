      *****************************************************************
      * NOME DA INC - I#GFCTK9                                        *
      * DESCRICAO   - PERMISSAO PARA ADESAO - CANCELAMENTO            *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTK9-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      * ALTERACAO   - VINICIUS - SOLICITACAO DE LEGADO, TARIFA OU     *
      *                AMBOS PARA ADESAO INDIVIDUAL EM OUTUBRO/2007   *
      *****************************************************************
       01  GFCTK9-ENTRADA.
           03 GFCTK9-LL                            PIC S9(004) COMP.
           03 GFCTK9-ZZ                            PIC S9(004) COMP.
           03 GFCTK9-TRANSACAO                     PIC  X(009).
           03 GFCTK9-FUNCAO                        PIC  X(008).
           03 GFCTK9-DADOS-RST.
              05 GFCTK9-QTDE-OCOR                  PIC  9(003).
              05 GFCTK9-QTDE-TOT-REG               PIC  9(006).
              05 GFCTK9-FUNC-BDSCO                 PIC  X(007).
              05 GFCTK9-PACOTE                     PIC  9(005).
              05 GFCTK9-AGPTO                      PIC  9(003).
              05 GFCTK9-FIM                        PIC  X(001).
              05 GFCTK9-TERMINAL                   PIC  X(008).
BI0912        05 FILLER                            PIC  X(236).
           03 GFCTK9-OCORR                         OCCURS 05 TIMES.
              05 GFCTK9-SEQUENCIA                  PIC  9(009).
              05 GFCTK9-CPF-CNPJ                   PIC  9(009).
              05 GFCTK9-FILIAL                     PIC  9(005).
              05 GFCTK9-CONTROLE                   PIC  9(002).
              05 GFCTK9-AGENCIA                    PIC  9(005).
              05 GFCTK9-CONTA                      PIC  9(013).
              05 GFCTK9-POSTO                      PIC  9(005).
              05 GFCTK9-SEGMENTO                   PIC  9(003).
              05 GFCTK9-MUNICIPIO                  PIC  9(007).
              05 GFCTK9-UF                         PIC  X(002).
              05 GFCTK9-DESC-NOME                  PIC  X(040).
              05 GFCTK9-TP-ADESAO                  PIC  X(001).
      **********************************************************
      * TIPO-ADESAO:  I - INDIVIDUAL  OU  C - COMPULSORIA
      **********************************************************
              05 GFCTK9-SOLICITA-EXCECAO           PIC  9(001).
BI0912        05 GFCTK9-PRMSS-INDVD                PIC  9(001).
      **********************************************************
      * SE TIPO-ADESAO = I - INDIVIDUAL
      *   PODERA SER:  0 - AMBOS  OU  1 - TARIFA  OU  2 - LEGADO
      **********************************************************
      * SE TIPO-ADESAO = C - COMPULSORIA
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
              05 GFCTK9-DATA-INICIO-ANT            PIC  X(010).
              05 GFCTK9-DATA-FIM-ANT               PIC  X(010).
              05 GFCTK9-DATA-INICIO                PIC  X(010).
              05 GFCTK9-DATA-FIM                   PIC  X(010).
              05 GFCTK9-ERRO-OCOR                  PIC  9(001).

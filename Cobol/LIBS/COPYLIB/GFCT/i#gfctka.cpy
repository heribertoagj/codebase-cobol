      *****************************************************************
      * NOME DA INC - I#GFCTKA                                        *
      * DESCRICAO   - PERMISSAO PARA ADESAO - CANCELAMENTO            *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTKA-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      * ALTERACAO   - VINICIUS - SOLICITACAO DE LEGADO, TARIFA OU     *
      *                AMBOS PARA ADESAO INDIVIDUAL EM OUTUBRO/2007   *
      *****************************************************************
       01  GFCTKA-SAIDA.
           03 GFCTKA-LL                            PIC S9(004) COMP.
           03 GFCTKA-ZZ                            PIC S9(004) COMP.
           03 GFCTKA-TRANSACAO                     PIC  X(009).
           03 GFCTKA-FUNCAO                        PIC  X(008).
           03 GFCTKA-DADOS-RST.
              05 GFCTKA-QTDE-OCOR                  PIC  9(003).
              05 GFCTKA-QTDE-TOT-REG               PIC  9(006).
              05 GFCTKA-FUNC-BDSCO                 PIC  X(007).
              05 GFCTKA-ERRO                       PIC  9(001).
              05 GFCTKA-COD-MSG-ERRO               PIC  9(007).
              05 GFCTKA-COD-SQL-ERRO               PIC  9(003).
              05 GFCTKA-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTKA-PACOTE                     PIC  9(005).
              05 GFCTKA-AGPTO                      PIC  9(003).
              05 GFCTKA-FIM                        PIC  X(001).
              05 GFCTKA-TERMINAL                   PIC  X(008).
BI0912        05 FILLER                            PIC  X(146).
           03 GFCTKA-OCORR                         OCCURS 05 TIMES.
              05 GFCTKA-SEQUENCIA                  PIC  9(009).
              05 GFCTKA-CPF-CNPJ                   PIC  9(009).
              05 GFCTKA-FILIAL                     PIC  9(005).
              05 GFCTKA-CONTROLE                   PIC  9(002).
              05 GFCTKA-AGENCIA                    PIC  9(005).
              05 GFCTKA-CONTA                      PIC  9(013).
              05 GFCTKA-POSTO                      PIC  9(005).
              05 GFCTKA-SEGMENTO                   PIC  9(003).
              05 GFCTKA-MUNICIPIO                  PIC  9(007).
              05 GFCTKA-UF                         PIC  X(002).
              05 GFCTKA-DESC-NOME                  PIC  X(040).
              05 GFCTKA-TP-ADESAO                  PIC  X(001).
      **********************************************************
      * TIPO-ADESAO:  I - INDIVIDUAL  OU  C - COMPULSORIA
      **********************************************************
              05 GFCTKA-SOLICITA-EXCECAO           PIC  9(001).
BI0912        05 GFCTKA-PRMSS-INDVD                PIC  9(001).
      **********************************************************
      * SE TIPO-ADESAO = I - INDIVIDUAL
      *   PODERA SER:  0 - AMBOS  OU  1 - TARIFA  OU  2 - LEGADO
      **********************************************************
      * SE TIPO-ADESAO = C - COMPULSORIA
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
              05 GFCTKA-DATA-INICIO-ANT            PIC  X(010).
              05 GFCTKA-DATA-FIM-ANT               PIC  X(010).
              05 GFCTKA-DATA-INICIO                PIC  X(010).
              05 GFCTKA-DATA-FIM                   PIC  X(010).
              05 GFCTKA-ERRO-OCOR                  PIC  9(001).

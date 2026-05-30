      *****************************************************************
      * NOME DA INC - I#GFCTKB                                        *
      * DESCRICAO   - PERMISSAO PARA ADESAO - CONSULTA                *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTKB-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      * ALTERACAO   - VINICIUS - SOLICITACAO DE LEGADO, TARIFA OU     *
      *                AMBOS PARA ADESAO INDIVIDUAL EM OUTUBRO/2007   *
      *****************************************************************
       01  GFCTKB-ENTRADA.
           03 GFCTKB-LL                            PIC S9(004) COMP.
           03 GFCTKB-ZZ                            PIC S9(004) COMP.
           03 GFCTKB-TRANSACAO                     PIC  X(009).
           03 GFCTKB-FUNCAO                        PIC  X(008).
           03 GFCTKB-DADOS-RST.
              05 GFCTKB-QTDE-OCOR                  PIC  9(003).
              05 GFCTKB-QTDE-TOT-REG               PIC  9(006).
              05 GFCTKB-FUNC-BDSCO                 PIC  X(007).
              05 GFCTKB-PONTEIRO.
                 07 GFCTKB-PONT-PACOTE             PIC  9(005).
                 07 GFCTKB-PONT-AGPTO              PIC  9(003).
                 07 GFCTKB-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTKB-PONT-CH-NUM7            PIC  9(013).
                 07 GFCTKB-PONT-CH-ALP             PIC  X(002).
                 07 GFCTKB-PONT-CH-DATA            PIC  X(010).
              05 GFCTKB-IND-DATA                   PIC  X(001).
      *       P-VIG/FUTURA T-TODAS
              05 GFCTKB-FIM                        PIC  X(001).
              05 GFCTKB-TERMINAL                   PIC  X(008).
BI0412        05 FILLER                            PIC  X(295).
           03 GFCTKB-OCORR                         OCCURS 05 TIMES.
              05 GFCTKB-SEQUENCIA                  PIC  9(009).
              05 GFCTKB-CPF-CNPJ                   PIC  9(009).
              05 GFCTKB-FILIAL                     PIC  9(005).
              05 GFCTKB-CONTROLE                   PIC  9(002).
              05 GFCTKB-AGENCIA                    PIC  9(005).
              05 GFCTKB-CONTA                      PIC  9(013).
              05 GFCTKB-POSTO                      PIC  9(005).
              05 GFCTKB-SEGMENTO                   PIC  9(003).
              05 GFCTKB-MUNICIPIO                  PIC  9(007).
              05 GFCTKB-UF                         PIC  X(002).
              05 GFCTKB-DESC-NOME                  PIC  X(040).
              05 GFCTKB-TP-ADESAO                  PIC  X(001).
      **********************************************************
      * TIPO-ADESAO: I - INDIVIDUAL  C - COMPULSORIA  A - AMBAS
      **********************************************************
              05 GFCTKB-SOLICITA-EXCECAO           PIC  9(001).
      **********************************************************
      * SE TIPO-ADESAO = C - COMPULSORIA  OU  A - AMBAS
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
BI0412        05 GFCTKB-PRMSS-INDVD                PIC  9(001).
BI0412**********************************************************
BI0412* SE TIPO-ADESAO = I - INDIVIDUAL   OU  A - AMBAS
BI0412*   PODERA SER:  0 - AMBOS  OU  1 - TARIFA  OU  2 - LEGADO
BI0412**********************************************************
              05 GFCTKB-DATA-INICIO                PIC  X(010).
              05 GFCTKB-DATA-FIM                   PIC  X(010).
              05 GFCTKB-GERA-CRED-CEL              PIC  9(001).
      **********************************************************
      * GERA CREDITO: 0 - NAO GERA   OU  1 - GERA CREDITO
      **********************************************************
BI0810        05 GFCTKB-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
BI0412        05 GFCTKB-VISUAL-COMPULS             PIC  9(001).
BI0412*           0  - NAO VISUALIZA COMPULSORIA
BI0412*           1  - VISUALIZA COMPULSORIA

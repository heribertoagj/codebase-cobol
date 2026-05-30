      *****************************************************************
      * NOME DA INC - I#GFCTKC                                        *
      * DESCRICAO   - PERMISSAO PARA ADESAO - CONSULTA                *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTKC-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      * ALTERACAO   - VINICIUS - SOLICITACAO DE LEGADO, TARIFA OU     *
      *                AMBOS PARA ADESAO INDIVIDUAL EM OUTUBRO/2007   *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
       01  GFCTKC-SAIDA.
           03 GFCTKC-LL                            PIC S9(004) COMP.
           03 GFCTKC-ZZ                            PIC S9(004) COMP.
           03 GFCTKC-TRANSACAO                     PIC  X(009).
           03 GFCTKC-FUNCAO                        PIC  X(008).
           03 GFCTKC-DADOS-RST.
              05 GFCTKC-QTDE-OCOR                  PIC  9(003).
              05 GFCTKC-QTDE-TOT-REG               PIC  9(006).
              05 GFCTKC-FUNC-BDSCO                 PIC  X(007).
              05 GFCTKC-PONTEIRO.
                 07 GFCTKC-PONT-PACOTE             PIC  9(005).
                 07 GFCTKC-PONT-AGPTO              PIC  9(003).
                 07 GFCTKC-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTKC-PONT-CH-NUM7            PIC  9(013).
                 07 GFCTKC-PONT-CH-ALP             PIC  X(002).
                 07 GFCTKC-PONT-CH-DATA            PIC  X(010).
              05 GFCTKC-ERRO                       PIC  9(001).
              05 GFCTKC-COD-MSG-ERRO               PIC  9(007).
              05 GFCTKC-COD-SQL-ERRO               PIC  9(003).
              05 GFCTKC-FIM                        PIC  X(001).
              05 GFCTKC-IND-DATA                   PIC  X(001).
      *       P-VIG/FUTURA T-TODAS
              05 GFCTKC-TERMINAL                   PIC  X(008).
BI0412        05 FILLER                            PIC  X(205).
           03 GFCTKC-OCORR                         OCCURS 05 TIMES.
              05 GFCTKC-SEQUENCIA                  PIC  9(009).
ST2507*       05 GFCTKC-CPF-CNPJ                   PIC  9(009).
ST2507*       05 GFCTKC-FILIAL                     PIC  9(005).
ST2507        05 GFCTKC-CPF-CNPJ                   PIC  X(009).
ST2507        05 GFCTKC-FILIAL                     PIC  X(004).
              05 GFCTKC-CONTROLE                   PIC  9(002).
              05 GFCTKC-AGENCIA                    PIC  9(005).
              05 GFCTKC-CONTA                      PIC  9(013).
              05 GFCTKC-POSTO                      PIC  9(005).
              05 GFCTKC-SEGMENTO                   PIC  9(003).
              05 GFCTKC-MUNICIPIO                  PIC  9(007).
              05 GFCTKC-UF                         PIC  X(002).
              05 GFCTKC-DESC-NOME                  PIC  X(040).
      **********************************************************
      * TIPO-ADESAO: I - INDIVIDUAL  C - COMPULSORIA  A - AMBAS
      **********************************************************
              05 GFCTKC-SOLICITA-EXCECAO           PIC  9(001).
      **********************************************************
      * SE TIPO-ADESAO = C - COMPULSORIA  OU  A - AMBAS
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
BI0412        05 GFCTKC-PRMSS-INDVD                PIC  9(001).
BI0412**********************************************************
BI0412* SE TIPO-ADESAO = I - INDIVIDUAL   OU  A - AMBAS
BI0412*   PODERA SER:  0 - AMBOS  OU  1 - TARIFA  OU  2 - LEGADO
BI0412**********************************************************
              05 GFCTKC-DATA-INICIO                PIC  X(010).
              05 GFCTKC-DATA-FIM                   PIC  X(010).
              05 GFCTKC-GERA-CRED-CEL              PIC  9(001).
      **********************************************************
      * GERA CREDITO: 0 - NAO GERA   OU  1 - GERA CREDITO
      **********************************************************
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
BI0412        05 GFCTKC-VISUAL-COMPULS             PIC  9(001).
BI0412*           0  - NAO VISUALIZA COMPULSORIA
BI0412*           1  - VISUALIZA COMPULSORIA
ST2507     03 FILLER                               PIC  X(005).

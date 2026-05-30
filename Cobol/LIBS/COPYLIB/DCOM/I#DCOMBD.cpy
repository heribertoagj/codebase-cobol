      ******************************************************************
      * SISTEMA : DCOM - I#DCOMBD                                      *
      * TAMANHO : 0500 BYTES                                           *
      * ARQUIVO : LAYOUT INTERMEDIARIO DA INTERFACE DCOM / NBDD        *
      *           ESTOQUE, NOVAS E LIMITE                              *
      *-----------------------------------------------------------------
      * OBS.....: HEADER E DETALHE DA INC I#NBDD03                     *
      *           FORMATO DAS DATAS: AAAAMMDD                          *
      *           CAMPOS ALFANUMERICOS: ALINHAR O CONTEUDO A ESQUERDA  *
      *           COMPLETAR SE NECESSARIO COM 'BRANCOS'                *
      *           EXEMPLO: INDEXADOR: 'PRE      '  PRE-FIXADO          *
      *                               '00034    '  NTAX                *
      *                               'CESTA01  '  PLATAFORMA BAIXA    *
      *-----------------------------------------------------------------
       01  DCOMBD-HEADER.
      *
         05  HBD-TP-REGISTRO               PIC 9(01).
      *         = 0 HEADER                                POS   1
         05  HBD-TP-ARQUIVO                PIC X(03).
      *         = 'EST' ESTOQUE (FECHADO COM O CONTABIL)  POS   2
      *         = 'NOV' MOVIMENTO DE ENTRADA DE OPERACOES NOVAS
      *         = 'GER' POSICAO GERENCIAL
         05  HBD-DT-HR-GERACAO             PIC X(26).
      *                                                   POS   5
      *         FORMATO TIMESTAMP: AAAA-MM-DD-HH.MM.SS.MMMMMM
         05  HBD-CCUSTO                    PIC X(04).
      *         CENTRO DE CUSTO                           POS  31
         05  HBD-DT-REFERENCIA             PIC 9(08).
      *         FORMATO AAAAMMDD                          POS  35
         05  HBD-NM-ARQUIVO                PIC X(08).
      *         NOME DO ARQUIVO                           POS  43
         05  HBD-FILLER                    PIC X(450).
      *                                                   POS  51
      *-----------------------------------------------------------------
       01  DCOMBD-DETALHE REDEFINES DCOMBD-HEADER.
      *
         05  DBD-TP-REGISTRO               PIC 9(01).
      *         = 2 DETALHE                               POS   1
      *         = 3 DETALHE (OPERACOES EM CA CREDITO EM ATRASO)
         05  DBD-CD-BANCO                  PIC 9(05).
      *         CODIGO DO BANCO, EX: 00237 - BRADESCO     POS   2
         05  DBD-CD-EMPRESA                PIC 9(05).
      *         CODIGO DA EMPRESA, EX:00237 - BRADESCO    POS   7
         05  DBD-CD-DEPTO                  PIC 9(05).
      *         CODIGO DO DEPARTAMENTO                    POS  12
         05  DBD-CD-PRODUTO                PIC X(09).
      *         CODIGO DO PRODUTO                         POS  17
         05  DBD-CD-SUB-PRODUTO            PIC X(09).
      *         CODIGO DO SUB-PRODUTO                     POS  26
         05  DBD-CD-CARTEIRA               PIC X(09).
      *         CODIGO DA CARTEIRA                        POS  35
         05  DBD-NM-SUB-PRODUTO            PIC X(40).
      *         NOME DO SUB-PRODUTO                       POS  44
         05  DBD-TP-CLASSE                 PIC 9(01).
      *         = 1 ATIVO                                 POS  84
      *         = 2 PASSIVO
         05  DBD-INTRAGRUPO                PIC X(01).
      *         'S' = SIM                                 POS  85
      *         'N' = NAO
         05  DBD-MOEDA-ORIG                PIC X(09).
      *         MOEDA ORIGEM                              POS  86
      *         PREENCHER COM 'REAL     ' CASO NAO SE APLIQUE
         05  DBD-INDEXADOR-ORIG            PIC X(09).
      *         INDEXADOR ORIGEM                          POS  95
         05  DBD-COSIF                     PIC 9(09).
      *         COSIF                                     POS 104
         05  DBD-PERC-INDEXADOR            PIC S9(09)V9(06) COMP-3.
      *         PERCENTUAL DO INDEXADOR                   POS 113
      *         PREENCHER COM ZEROS CASO NAO SE APLIQUE
         05  DBD-RATING                    PIC X(03).
      *         RATING                                    POS 121
         05  DBD-SEGMENTO                  PIC 9(03).
      *         SEGMENTO                                  POS 124
         05  DBD-PAIS-ORIG                 PIC X(09).
      *         CODIGO DO PAIS ORIGEM                     POS 127
      *         PREENCHER COM '01058    ' CASO NAO SE APLIQUE
         05  DBD-TRAVA                     PIC X(09).
      *         TRAVA                                     POS 136
      *         PREENCHER COM 'SECAS    ' CASO NAO SE APLIQUE
         05  DBD-PORTFOLIO                 PIC X(01).
      *         'B' = BANKING                             POS 145
      *         'T' = TRADING
         05  DBD-MERCADORIA-ORIG           PIC X(09).
      *         TIPO DE MERCADORIA ATRELADO A ORIGEM      POS 146
      *         (COMMODITIES)
         05  DBD-DT-VENCTO-PC              PIC 9(08).
      *         DATA VENCIMENTO DA PARCELA                POS 155
         05  DBD-VLR-PRINC-PC              PIC S9(15)V9(02) COMP-3.
      *         VALOR PRINCIPAL DA PARCELA                POS 163
         05  DBD-VLR-PRINC-ORIG-PC         PIC S9(15)V9(02) COMP-3.
      *         VALOR PRINCIPAL ORIGINAL DA PARCELA       POS 172
         05  DBD-VLR-PRESENTE              PIC S9(15)V9(02) COMP-3.
      *         VALOR PRESENTE                            POS 181
         05  DBD-VLR-FUTURO                PIC S9(15)V9(02) COMP-3.
      *         VALOR FUTURO                              POS 190
         05  DBD-VLR-PRES-CUSTO            PIC S9(15)V9(02) COMP-3.
      *         VALOR PRESENTE CUSTO                      POS 199
         05  DBD-VLR-FUT-CUSTO             PIC S9(15)V9(02) COMP-3.
      *         VALOR FUTURO CUSTO                        POS 208
         05  DBD-VLR-PRES-FUNDING          PIC S9(15)V9(02) COMP-3.
      *         VALOR PRESENTE FUNDING                    POS 217
         05  DBD-VLR-FUT-MOEDA-ESTRANG     PIC S9(15)V9(02) COMP-3.
      *         VALOR FUTURO EM MOEDA ESTRANGEIRA         POS 226
         05  DBD-FILLER                    PIC X(66).
      *                                                   POS 235
      ******************************************************************
      **               C A M P O S   A U X I L I A R E S  - TAM = 200 **
      ******************************************************************
301 08   05  DBD-DT-INICIO-OPER            PIC  9(08).
309 08   05  DBD-DT-FINAL-OPER             PIC  9(08).
317 04   05  DBD-RAZAO-CTABIL.
317 02       10  DBD-RZ-GP-CTABIL          PIC  9(02).
319 02       10  DBD-RZ-SBGP-CTABIL        PIC  9(02).
321 07   05  DBD-CTA-CONTABIL              PIC  9(07).
328 01   05  DBD-DIG-CTA-CONTABIL          PIC  X(01).
329 09   05  DBD-CPFCNPJ                   PIC  9(09).
338 04   05  DBD-CPFCNPJ-FILIAL            PIC  9(04).
342 02   05  DBD-CPFCNPJ-CONTROLE          PIC  9(02).
      ******************************************************************
      **     VALORES DO ARQUIVO LIMITE                                **
      ******************************************************************
344 09   05  DBD-VALOR-LIMITE              PIC S9(15)V99   COMP-3.
353 09   05  DBD-VALOR-UTILIZADO           PIC S9(15)V99   COMP-3.
      ******************************************************************
      **     INFORMACOES DA OPERACAO                                  **
      ******************************************************************
362 09   05  DBD-VPRINC-PCELA-DESC         PIC S9(15)V99   COMP-3.
371 02   05  DBD-CSIT-DESC-COML            PIC S9(03)      COMP-3.
373 03   05  DBD-DANO-OPER-DESC            PIC S9(04)      COMP-3.
376 05   05  DBD-NSEQ-OPER-DESC            PIC S9(09)      COMP-3.
381 03   05  DBD-NPCELA-DESC-COML          PIC S9(05)      COMP-3.
384 10   05  DBD-TAXA-CUSTO-ANO            PIC S9(03)V9(15)  COMP-3.
394 08   05  DBD-INDICE-REF                PIC S9(06)V9(09)  COMP-3.
401 08   05  DBD-INDICE-OPE                PIC S9(06)V9(09)  COMP-3.
409 03   05  DBD-CAG-BCRIA                 PIC S9(05) COMP-3.
412 07   05  DBD-CCTA-BCRIA-CLI            PIC S9(13) COMP-3.
419 03   05  DBD-QTD-DIA-PRESENTE          PIC S9(05) COMP-3.
422 03   05  DBD-QTD-DIA-FUTURO            PIC S9(05) COMP-3.
425 03   05  DBD-CELMTO-DESC-COML          PIC 9(003).
428 72   05  FILLER                        PIC  X(72).

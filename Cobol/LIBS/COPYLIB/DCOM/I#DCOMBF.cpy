      ******************************************************************
      * SISTEMA : DCOM - I#DCOMBF                                      *
      *-----------------------------------------------------------------
      * COPIA DA INC I#NBDD07                                          *
      *-----------------------------------------------------------------
      *    INC  ****  I#NBDD07  - ARQUIVO DE INTERFACE NBDD            *
      *                           LAYOUT 07                            *
      *               FB - TAMANHO = 300 BYTES                         *
      *                                                                *
      *    OBS. FORMATO DAS DATAS: AAAAMMDD                            *
      *         CAMPOS ALFANUMERICOS: ALINHAR O CONTEUDO A ESQUERDA E  *
      *         COMPLETAR SE NECESSARIO COM 'BRANCOS'                  *
      *         EXEMPLO: INDEXADOR: 'PRE      '  PRE-FIXADO            *
      *                             '00034    '  NTAX                  *
      *                             'CESTA01  '  PLATAFORMA BAIXA      *
      *                                                                *
      *-----------------------------------------------------------------
      *         MANUTENCAO:                                            *
      *         DATA.... -BREVE DESCRICAO............................. *
      *                                                                *
      *-----------------------------------------------------------------
       01  HEADER-07.
      *
         05  H07-TP-REGISTRO               PIC 9(01).
      *         = 0 HEADER                                POS   1
         05  H07-TP-ARQUIVO                PIC X(03).
      *         = 'LIM' LIMITES OPERACIONAIS E ROTATIVOS  POS   2
         05  H07-DT-HR-GERACAO             PIC X(26).
      *                                                   POS   5
      *         FORMATO TIMESTAMP: AAAA-MM-DD-HH.MM.SS.MMMMMM
         05  H07-CCUSTO                    PIC X(04).
      *         CENTRO DE CUSTO                           POS  31
         05  H07-DT-REFERENCIA             PIC 9(08).
      *         FORMATO AAAAMMDD                          POS  35
         05  H07-NM-ARQUIVO                PIC X(08).
      *         NOME DO ARQUIVO                           POS  43
         05  H07-FILLER                    PIC X(250).
      *                                                   POS  51
      *-----------------------------------------------------------------
       01  DETALHE-07 REDEFINES HEADER-07.
      *
         05  R07-TP-REGISTRO               PIC 9(01).
      *         = 1 SEM MOVIMENTO                         POS   1
      *         = 2 DETALHE
      *         = 3 DETALHE (OPERACOES EM CA CREDITO EM ATRASO)
         05  R07-CD-BANCO                  PIC 9(05).
      *         CODIGO DO BANCO, EX: 00237 - BRADESCO     POS   2
         05  R07-CD-EMPRESA                PIC 9(05).
      *         CODIGO DA EMPRESA, EX:00237 - BRADESCO    POS   7
         05  R07-CD-DEPTO                  PIC 9(05).
      *         CODIGO DO DEPARTAMENTO                    POS  12
         05  R07-CD-PRODUTO                PIC X(09).
      *         CODIGO DO PRODUTO                         POS  17
         05  R07-CD-SUB-PRODUTO            PIC X(09).
      *         CODIGO DO SUB-PRODUTO                     POS  26
         05  R07-CD-CARTEIRA               PIC X(09).
      *         CODIGO DA CARTEIRA                        POS  35
         05  R07-NM-SUB-PRODUTO            PIC X(40).
      *         NOME DO SUB-PRODUTO                       POS  44
         05  R07-SEGMENTO                  PIC 9(03).
      *         SEGMENTO                                  POS  84
         05  R07-DT-VENCTO                 PIC 9(08).
      *         DATA DE VENCIMENTO DO LIMITE              POS  87
         05  R07-DT-CONTRATACAO-LIM        PIC 9(08).
      *         DATA DA CONTRATACAO/RENOVACAO OU          POS  95
      *         ADITAMENTO DO LIMITE
         05  R07-VLR-LIMITE                PIC S9(15)V9(02) COMP-3.
      *         VALOR LIMITE DE CREDITO CONCEDIDO         POS 103
         05  R07-VLR-UTILIZADO             PIC S9(15)V9(02) COMP-3.
      *         VALOR UTILIZADO DO LIMITE                 POS 112
MSENA    05  R07-CELMTO-DESC-COML          PIC 9(003).
MSENA *         CODIGO DO ELEMENTO                        POS 121
         05  R07-FILLER                    PIC X(177).
      *                                                   POS 124
      *-----------------------------------------------------------------
      *  FIM DA INC I#DCOMBF


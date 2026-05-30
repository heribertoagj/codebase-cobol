      *----------------------------------------------------------------*
      *    INC 'I#SIMM01'       - AREA DE COMUNICACAO DE MENSAGENS     *
      *    TAMANHO              - 0186 BYTES                           *
      *                                                                *
      *    ANALISTA RESPONSAVEL - MARCILENE/INES(PRIME)                *
      *    ATUALIZACAO          - 18/07/2003                           *
      *                           INCLUSAO DO CAMPO HOPER-ARQ-DISPN    *
      *    DESCRICAO            - BLOCO DE CONTROLE BRADESCO           *
      *----------------------------------------------------------------*
      *
       01  SIMM01-REGISTRO.
           03  SIMM01-BLOCO-CONTROLE.
               05  SIMM01-IBLOCO            PIC  X(0008).
               05  SIMM01-GQPOSIC.
                   07  SIMM01-QPOSIC-OPER   PIC  9(0005).
               05  SIMM01-GCEMPR.
                   07  SIMM01-CEMPR-INC     PIC  9(0005).
               05  SIMM01-IEMPR-INC         PIC  X(0020).
               05  SIMM01-TPO-PAPEL         PIC  X(0015).
               05  SIMM01-GPRAZO.
                   07  SIMM01-PRAZO         PIC  9(0005).
               05  SIMM01-GDTEMISS.
                   07  SIMM01-DTEMISS       PIC  9(0008).
               05  SIMM01-GQTDE.
                   07  SIMM01-QTDE-NEGOCIO  PIC  9(0009).
               05  SIMM01-GTX.
                   07  SIMM01-TX-NEGOCIO    PIC  9(0009).
               05  SIMM01-GCVLR-NEG.
                   07  SIMM01-CVLR-NEG      PIC  9(0002).
                   07  SIMM01-VLR-NEGOCIO   PIC  9(0015)V99.
               05  SIMM01-CTRANS-ORIGE      PIC  X(0004).
               05  SIMM01-CCTRO-CUSTO       PIC  X(0004).
               05  SIMM01-CPROG-ORIGE       PIC  X(0008).
               05  SIMM01-CUSUAR-SENHA      PIC  X(0007).
               05  SIMM01-GCDEPDC.
                   07  SIMM01-CDEPDC-ORIGE  PIC  9(0005).
               05  SIMM01-CCTRL-ORIGE       PIC  X(0030).
               05  SIMM01-HOPER-ARQ-DISPN   PIC  X(0020).
               05  FILLER                   PIC  X(0005).

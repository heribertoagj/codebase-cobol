      *================================================================*
      * NOME BOOK : BVVEW701                                           *
      * DESCRICAO : COMUNICACAO DO PROGRAMA BVVE3701                   *
      * DATA      : 21/12/2020                                         *
      * ANALISTA  : MAXIMILIANO BARBOSA                                *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * TAMANHO   : 1059 BYTES                                         *
      *================================================================*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * ##/##/#### XXXXXXXX/XXXXX - XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *================================================================*
      *
       05  BVVEW701-REGISTRO.
      *
           10  BVVEW701-ENTRADA.
               15 BVVEW701-QNT-NCTRL-PROCM-ARQ  PIC 9(009).
               15 BVVEW701-NCTRL-PROCM-ARQ-OCC  OCCURS 15 TIMES.
                   20 BVVEW701-NCTRL-PROCM-ARQ  PIC 9(008).
           10  BVVEW701-SAIDA.
               15 BVVEW701-QNT-NMNTRC-TRFGO-ARQ PIC 9(009).
               15 BVVEW701-NMNTRC-TRFGO-ARQ-OCC OCCURS 15 TIMES.
                   20 BVVEW701-NMNTRC-TRFGO-ARQ PIC 9(018).
                   20 BVVEW701-ISPB-ORIGEM-ARQ  PIC X(008).
                   20 BVVEW701-ISPB-DESTINO-ARQ PIC X(008).
      *
           10  BVVEW701-RETORNO.
               15  BVVEW701-COD-RETORNO    PIC  9(002).
               15  BVVEW701-CHAVE-DB2      PIC  X(050).
               15  BVVEW701-MSG-RETORNO    PIC  X(079).
               15  BVVEW701-ERRO-AREA      PIC  X(520).

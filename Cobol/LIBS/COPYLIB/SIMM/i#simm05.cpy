      *----------------------------------------------------------------*
      *    INC 'I#SIMM05'       - AREA DE COMUNICACAO DE MENSAGENS     *
      *    TAMANHO              - 32768 BYTES                          *
      *                                                                *
      *    ANALISTA RESPONSAVEL - MARCILENE/INES(PRIME)                *
      *    ATUALIZACAO          - 09/08/2001                           *
      *    DESCRICAO            - BLOCO DE SISMSG                      *
      *----------------------------------------------------------------*
      *
       01  SIMM05-REGISTRO.
           03  SIMM05-BLOCO-CONTROLE.
               05  SIMM05-MSG               PIC  X(32768).

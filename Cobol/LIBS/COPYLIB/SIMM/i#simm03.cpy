      *----------------------------------------------------------------*
      *    INC 'I#SIMM03'       - BLOCO DE RETORNO                     *
      *    TAMANHO              - 217  BYTES                           *
      *                                                                *
      *    ANALISTA RESPONSAVEL - INES/MARCILENE(PRIME)                *
      *    ATUALIZACAO          - 09/08/2001                           *
      *    DESCRICAO            - LAYOUT DE BLOCO DE RETORNO           *
      *----------------------------------------------------------------*
      *
       01  SIMM03-REGISTRO.
           03  SIMM03-BLOCO-RETORNO-BDSCO.
               05  SIMM03-ID-BLOCO-X        PIC  X(0008).
               05  SIMM03-CNRO-CTRL-INSTC-X PIC  X(0020).
               05  SIMM03-CNRO-UNIC-SIMM-X  PIC  X(0023).
               05  SIMM03-CRETOR-PRINC-N    PIC  9(0004).
               05  SIMM03-CRETOR-OPCAO-N    PIC  9(0004).
               05  SIMM03-CNRO-UNIC-OPER-X  PIC  X(0023).
               05  SIMM03-CCTRL-ORIGE       PIC  X(0030).
               05  SIMM03-RMSGEM-ERRO-X     PIC  X(0080).
               05  SIMM03-RESERVADO-SIMM    PIC  X(0023).
               05  FILLER                   PIC  X(0002).

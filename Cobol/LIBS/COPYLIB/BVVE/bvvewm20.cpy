      *----------------------------------------------------------------*
      *    INC 'BVVEWM20'       - BLOCO CONTROLE MONITORACAO           *
      *    TAMANHO              - 660 BYTES                            *
      *                                                                *
      *----------------------------------------------------------------*
      *
           05  BVVEWM20-REGISTRO.
               10  BVVEWM20-TAM-POSIC          PIC S9(0008) COMP.
               10  BVVEWM20-TAM-JSON           PIC S9(0008) COMP.
               10  BVVEWM20-CODMSG             PIC  X(0009).
               10  BVVEWM20-NUM-UNICO-SIST     PIC  X(0023).
               10  BVVEWM20-NUOP               PIC  X(0023).
               10  BVVEWM20-CTRLIF             PIC  X(0020).
               10  BVVEWM20-CEMISR-MSGEM       PIC  X(0008).
               10  BVVEWM20-CDSTNA-MSGEM       PIC  X(0008).
               10  BVVEWM20-HOCORR-WKFLW       PIC  X(0026).
               10  BVVEWM20-CDOM-MSGRA-SPB     PIC  X(0005).
               10  BVVEWM20-ENTRADA-MANUAL     PIC  X(0001).
               10  BVVEWM20-USUARIO            PIC  X(0009).
               10  BVVEWM20-MONITOR.
                   15  BVVEWM20-STATUS-CONV    PIC  9(01).
                   15  BVVEWM20-MSGERR-CONV    PIC  X(255).
                   15  BVVEWM20-STATUS-OPEN    PIC  9(01).
                   15  BVVEWM20-MSGERR-OPEN    PIC  X(255).
               10  BVVEWM20-RESERVA            PIC  X(08).

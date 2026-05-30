      *****************************************************************
      * I#DCOMBH       LEIAUTE PARA OBTENCAO DE RATING E SEGMENTO     *
      * COPIA DA I#NBDD30                                             *
      *     LRECL = 030     -----------------  MAINFRAME              *
      *===============================================================*
      * CRIADA EM 04/06/2010 - POR: CARLOS GARCIA      - LRECL = 030  *
      * OBJETIVO: DISPONIBILIZAR RATING E SEGMENTO PARA AS INTERFACES *
      *****************************************************************
       01  D30-REGISTRO.
           03 D30-CD-CPF-CNPJ            PIC  9(09).
           03 D30-CD-FILIAL              PIC  9(04).
           03 D30-CD-DIGITO              PIC  9(02).
           03 D30-CD-SEGMENTO            PIC  9(03).
           03 D30-CD-RATING              PIC  X(03).
           03 FILLER                     PIC  X(09).

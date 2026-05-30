      ********************************************************
      * IMAGEM DA TABELA SLIGB09?                 LRECL=107  *
      ********************************************************
       01  REG-SLIGB09X.
           10 SLIG9F-CCGC-CPF-CLI      PIC S9(09)      COMP-3.
           10 SLIG9F-CINDCD-PSSOA-CLI  PIC  X(01).
           10 SLIG9F-CCGC-CPF-SACDO    PIC S9(09)      COMP-3.
           10 SLIG9F-CINDCD-PSSOA-SACDO
                                       PIC X(01).
           10 SLIG9F-CIDTFD-CART-LIQDZ PIC S9(02)      COMP-3.
           10 SLIG9F-DPER-ABRGC        PIC X(10).
           10 SLIG9F-VANTES-PER-NAO-PG PIC S9(13)V99   COMP-3.
           10 SLIG9F-VANTES-PER-NAO-PG-N
                                       PIC X(01).
           10 SLIG9F-VVCTO-PER-PG      PIC S9(13)V99   COMP-3.
           10 SLIG9F-VVCTO-PER-PG-N    PIC     X(01).
           10 SLIG9F-VVCTO-PER-NAO-PG  PIC S9(13)V99   COMP-3.
           10 SLIG9F-VVCTO-PER-NAO-PG-N
                                       PIC X(01).
           10 SLIG9F-VVCTO-PER-BXADO   PIC S9(13)V99   COMP-3.
           10 SLIG9F-VVCTO-PER-BXADO-N PIC X(01).
           10 SLIG9F-VVCTO-APOS-PER-PG PIC S9(13)V99   COMP-3.
           10 SLIG9F-VVCTO-APOS-PER-PG-N
                                       PIC X(01).
           10 SLIG9F-VAPOS-PER-NAO-PG  PIC S9(13)V99   COMP-3.
           10 SLIG9F-VAPOS-PER-NAO-PG-N
                                       PIC X(01).
           10 SLIG9F-VAPOS-PER-BXADO   PIC S9(13)V99   COMP-3.
           10 SLIG9F-VAPOS-PER-BXADO-N PIC X(01).
           10 SLIG9F-VTOT-PRROG-TITLO  PIC S9(13)V99   COMP-3.
           10 SLIG9F-VTOT-PRROG-TITLO-N
                                       PIC X(01).
           10 SLIG9F-VTOT-ABTMT-TITLO  PIC S9(13)V99   COMP-3.
           10 SLIG9F-VTOT-ABTMT-TITLO-N
                                       PIC X(01).
           10 SLIG9F-CNRO-PTCAO-HIST   PIC S9(03)     COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           10 SLIG9F-CCGC-CPF-CLI-ST   PIC X(009).
           10 SLIG9F-CCGC-CPF-SACDO-ST PIC X(009).

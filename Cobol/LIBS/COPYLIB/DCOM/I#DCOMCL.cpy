      *----------------------------------------------------------------*
      * SISTEMA = DCOM - CLLP               BOOK     =  I#DCOMCL       *
      * ARQUIVO = LAYOUT DE TRANSFERENCIA PARA LP, PARA ENVIO MENSAL   *
      *           AO SISTEMA CLLP           SEQUENCIAL                 *
      * DCOM    = 301 BYTES                 19-09-2008                 *
      *----------------------------------------------------------------*
       01 CL-REGISTRO.
001 02    05  CL-FIXO                    PIC  9(002).
003 04    05  CL-AGENCIA                 PIC  9(004).
007 07    05  CL-NR-CC                   PIC  9(007).
014 04    05  CL-SIGLA                   PIC  X(004).
018 03    05  CL-CART                    PIC  X(003).
021 07    05  CL-NRO-TIT                 PIC  9(007).
028 08    05  CL-DTVCTO                  PIC  9(008).
036 08    05  CL-DTINI                   PIC  9(008).
044 08    05  CL-MOVTO                   PIC  9(008).
052 02    05  CL-MOEDA                   PIC  X(002).
054 02    05  CL-GARAN                   PIC  9(002).
056 01    05  CL-MARCA                   PIC  X(001).
057 02    05  CL-MOTIVO                  PIC  9(002).
059 30    05  CL-DUP-PRO                 PIC  X(030).
089 15    05  CL-VR-TIT                  PIC  9(013)V9(02).
104 15    05  CL-VR-CONT-BRU             PIC  9(013)V9(02).
119 15    05  CL-VRRAP                   PIC  9(013)V9(02).
134 15    05  CL-VR-EN-TX                PIC  9(013)V9(02).
149 15    05  CL-VR-IOF                  PIC  9(013)V9(02).
164 40    05  CL-DEV-RESP                PIC  X(040).
204 40    05  CL-NOME-COOB1              PIC  X(040).
244 40    05  CL-NOME-COOB2              PIC  X(040).
284 15    05  CL-CGCCPF.
284 09       10  CL-CGCNUM               PIC  9(009).
293 04       10  CL-CGCFIL               PIC  9(004).
297 02       10  CL-CGCCTR               PIC  9(002).
299 03    05 CL-NATOPER                  PIC  9(003).

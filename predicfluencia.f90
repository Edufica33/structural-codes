PROGRAM Program_LateralForce_axialLoad
	!Elaborado por Campos, Moo, Alcocer, Rosado.
	!Fechad de la última modificación: 20/09/22
	!OBJETIVO: obtener los puntos para el diagrama de interacción de una sección rectangular con N lechos de acero
	!UNIVERSIDAD AUTÓNOMA DE YUCATÁN.
	!CAMPUS DECIENCIAS EXACTAS E INGENIERÍAS.
	!FACULTAD DE INGENIERÍA.
	!MAESTRÍA EN INGENIERÍA.
	!OPCIÓN ESTRUCTURAS.

IMPLICIT NONE

!Definición de variables
	!datos de la sección
        REAL:: b,hc,Ig, lm, hm
            !ancho muro- ancho castillo, altura castillo, inercia gruesa,largo del muro, altura del muro

	!datos de los materiales
        REAL::fpc,fc,Es,fs,ft,Ec,n, EPSIcu, EPSIcuCon, Fr, EcCCA, fpcCCA, EPSIcuCCA, h  
            !f'c, esfuerzo en el concreto, mod. de elasticidad del acero, 
            !resistencia a tensión por flexión del concreto, modulo de elasticidad del concreto, relación modal, def. ultima concreto, def. ultima concreto confinado. 
        REAL:: k,RHOs,fyh,Vsh,Vcc,Sh, diametroh, bpk ,hpk, Zm, EPSIcero, EPSIceroCon, EPSIfr
            !factor k, relación entre volúmenes, fluencia del acero transversal, Vol. de refuerzo trans 
            !Vol. de concreto confinado, separación entre estribos, Area de estribos
            !largo de los estribos, alto de los estribos, pendiente de la recta del modelo, deformación en la resistencia máxima del concreto no confinado
        CHARACTER(len=50):: TipoAcero  !tipo de acero que se usará
        REAL :: FY, EPSIsu
            !Fluencia del acero, deformación última del acero
        REAL:: AsE(20,3) ,As(20,2), Ast, Ag
            !AsE: matriz de entrada donde primera columna es el diámetro de la barra, segunda es el número de barras por lecho y tercero la profundidad
            !As: matriz que engloba el área  en la primera columna y la profundidad en la segunda columna (máximo 20 lechos)
        REAL:: SIGMAcon, SIGMACCA
	!datos a calcular de la sección
        REAL:: EPSIj, EPSIex, MPNC(3000,4), MPC(3000,5), EPSIt1
            !Deformación en el donde se cambia de concreto no confinadao a confinado, deformación en el extremo de compresión. 
            !matriz del diagrama de interacción falla concreto no confi. 1 col=momento; 2 col=carga axial con def. max. conc. no conf.; 
            !matriz del diagrama de interacción falla concreto confi.1 col= momento; 2 col=carga axial con def. max de conc. conf.; 3 col: centrimetros de concreto no confinado que se perdieron
        REAL:: EPSIs,EPSIc, rho, rc
        !def. en el acero, def. en el concreto, porcentaje de acero, distancia de los extremos de la sección a los límites del núcleo confinado
        INTEGER :: NL !número de lechos de acero

	!Variables auxiliares
        INTEGER:: i, cont, cont2, bandera2, cont3
        REAL:: sumCcr1, noCon, SUMIcr
        CHARACTER(len=50):: mensaje, nombre
        REAl:: PHIn, Cn, P
        REAL:: Fcn1, Fcn2, Fcn3,a, z, maxI, Fsc, Fst, PM, Fncex, Fncj, EN1, Msc, Mst, ft1, ft2, ft3, ft4
        REAL:: x, y
        REAL:: FibrasCon(2000,6), NF, DF, contPF, Cf, hfrac, MC(2500,4), Asn(20,5), Mcc, Fcc, Mn, PHIb, Cm
        REAL:: hfrac2, mct1, mct2, mct3
        INTEGER:: bandera, 	contPHI, NMaxPHI
        CHARACTER (len=50):: Criterio

	!Nombres de las funciones
        REAL:: fckp, EPSIkp, fsm, Ftn, CT, TT, Cn, tol
    
!LECTURA DEL ARCHIVO DE ENTRADA

	OPEN(UNIT=11, FILE='datos.txt', STATUS='OLD', ACTION='READ')
	 READ(11,*) 
	 READ(11,*) 
	 READ(11,*)nombre
     READ(11,*) 
	 READ(11,*)lm,hm, b, hc
	 READ(11,*) 
     READ(11,*)fpc, Fr, EcCCA, fpcCCA 
	 READ(11,*)
     READ(11,*)NL, TipoAcero
     READ(11,*)
	 do i=1,NL
     	READ(11,*) AsE(i,1),  AsE(i,2),  AsE(i,3)   !diametro, numero de barras , profundidad
	 end do
	     
    CLOSE(11)

Print*, b,lm, hm, hc, fr, EcCCA,fpc, TipoAcero
!Impresión en CMD
	do i=1,NL
     	Print*, AsE(i,1),  AsE(i,2),  AsE(i,3)
	end do

!Se crea la matriz As
 do i=1,NL
    As(i,1)= (3.14159265*((AsE(i,1)/2)**2))* AsE(i,2)  !area de acero por lecho
	As(i,2)= AsE(i,3)  !profundidad del lecho
 end do
	
!hipótesis 
 !Cálculos preliminares    
        Ag=b*lm  !area gruesa de la sección a flexión
        Ast=0
		!cálculo del area total del acero
			do i=1, NL
				Ast=Ast+As(i,1)    
			end do
                  
        noCon=1
 		Ec=11236*SQRT(fpc)  !11236*SQRT(fpc)
 		Es=2000000
		n=Es/Ec
        rho=Ast/Ag		!calculo del porcentaje de acero
        EPSIcu=0.004
 !se asignan los valores del acero
    if (TipoAcero== "G42") then 
        fy=4200     !esfuerzo nominal a tensión
		EPSIsu=0.1295
    end if 
    if (TipoAcero== "G42RB") then 
        fy=4593     
		EPSIsu=0.1295
    end if 
    if (TipoAcero== "G42T") then 
        fy=5219     
		EPSIsu=0.175
    end if 
    if (TipoAcero== "G56") then 
        fy=5600     
		EPSIsu=0.15
    end if 
    if (TipoAcero== "G56T") then 
        fy=5887.9     
		EPSIsu=0.15
    end if 

    
	 

!PASO 1. Calcular los puntos con compresión pura 	
	!!Para el criterio de falla del CCA
		!se calculas las deformaciones en los puntos de interés en el concreto y sus respectivos esfuerzos
        !todos tienen el mismo nivel de deformación
		!se calculan las fuerzas 
			!!!fuerzas de compresión de concreto
                EPSIcuCCA=fpcCCA/EcCCA   !Deformación última del CCA a compresión
				SIGMACCA=fpcCCA !esfuerzo máximo a compresión del CCA
				SIGMAcon=fckp(noCon,fpc,EPSIcuCCA,zm)    !esfuerzo del concreto de los castillos
				Fcn1= SIGMAcon*(hc*b)*2  !fuerza en los castillos 
				Fcn2= SIGMACCA*(lm-2*hc)*b  !fuerza los bloques de CCA
			!fuerzas de compresión en el acero
				Fsc=0   !sumatoria de fuerzas de los aceros
				do i=1, NL
					Fsc=Fsc+ (As(i,1)*fsm(TipoAcero,(EPSIcuCCA),Es))  !la deformación es la misma en todo
				end do
			!se saca la sumatoria de fuerzas
				CT=Fcn1+Fcn2+fsc
		!La P debe ser igual a toda la compresión que pueda soportar la sección
			MPC(1,1)=0   !la carga lateral es 0
			MPC(1,2)=CT/(b*lm) 		!el esfuerzo axial, es igual a la carga lateral /area tributaria
            MPC(1,3)=2.5*lm !un número muy grande, cercano a infinito	
            MPC(1,4)=0    !no hay ángulo 

	
	
!PASO 2. se barre con valores de C para obtener los puntos de M y P	

	!Se calcula el centroide transformado
	P=2   !se le da un valor inicial mayor que 1 para que no saque la iteración
	hfrac2=0
	cont=1 !cuantas iteraciones para hallar C, empieza en 1 porque el primer valor se calcular en el paso 1
	cont2=1	 !contador por si hay un ciclado
	maxI=500 !número máxim0o de iteraciones
	Cn=2*lm  !la C se vuelve la media los límites 
	!se aplican las fórmulas para obtener el equilibrio a partir del centroide
	bandera2=0  !esta indica si pasó a la zona de cambio de criterio
	bandera=0
    EPSIfr=(fr/2)/Ec  !deformación de fractura del concreto a tensión por flexión
	print*, EPSIfr
	
 10	do
		if (bandera==0) then   !solo se cambia cuando no hay fallos , si algo falla se repite el Cn
			if (Cn .GT. (2*Lm))  then !para Cn grandes su reducción es mayor
				Cn=Cn-0.1*Cn
			else
				if ( Cn<(Lm+1) ) then
					if (Cn .LE. 0) then !si parte a la parte de pura tensión elástica, es una linea recta no vale la pena ser muy finos
						Cn=Cn-0.5
					else  !si está  en zona 0<C<h
						Cn=Cn-0.1
					end if
				else
					Cn=Cn-2
				end if	
			end if
		end if
		
		if ( bandera2==1 ) then!pregunta si ya falló el concreto del castillo a tensión por primera vez
			if ( Cn .LE. 0) then !pregunta si el eje neutro ya es negativo, si ya todo está a tensión
				Phin= (EPSIfr)/(lm-Cn) !cuando todo está a tensión el criterio es la fluencia del acero
				mensaje= '   Fractura concreto 2'
			else  !si sigue ne la zona donde hay concreto se respeta el criterio de ruptura del acero
				Phin=(EPSIfr)/(lm-Cn)   !criterio de falla del acero	
				mensaje = '   Fractura concreto '
			end if
			
		else !aún no falla el concreto a tensión
            PHIn=EPSIcuCCA/(Cn-hc) !criterio de falla concreto fractura del CCA, se encuentra justo después del primer castillo
			mensaje='    fcu CCA'
		end if
		
		if (cont2>maxI) then
			print*, "se alcanzó el máximo de ciclos"
			exit
			
		end if
		  
		Criterio="No falla"   !se resetea para cada C para que solo aplique para la ultima C 
		bandera=0      !se inicia en 0 porque no hay falla todavía
		hfrac=0  !se resetea cual es la altura fractura para cada C, altura del concreto s
		Fcc=0   !se resetea aquí porque cunado haya tensión pura, el concreto no tendrá fuerzas
			if ( Cn .GT. 0 ) then  !si el eje neutro es negativo, se elimina al concreto de la ecuación
				!paso 7. Discretizar la zona de compresión de la sección transversal en fibras.  Considerar zonas de concreto confinado y no confinado 
					NF=2000  !número de fibras
					if (Cn>Lm) then
						DF=Lm/NF   !altura de fibra, se limita a toda la sección, la cual toda está a compresión.
					else	
						DF=Cn/NF   !aquí no toda la sección está a compresión.
					end if
				!PASO 8. Calcular la profundidad del centroide de cada fibra a compresión.
					contPF=0
					do i=1 ,NF
						FibrasCon(i,1)=contPF+((DF/2))!se obtiene la profundidad de cada fibra 
						contPf=(DF*i)      !esto hará que el proximo este DF/2 más abajo de la parte mas baja de la fibra anterior
					end do
				!print *,' profundidad', FibrasCon(1,1), FibrasCon(2,1), FibrasCon(int(NF),1)
				!Paso 9. Calcular la deformación del concreto ∈_c en el centroide de cada fibra a compresión
					do i=1 ,NF
						FibrasCon(i,2)=PHIn*(Cn-FibrasCon(i,1))  !se obtiene la def. en el centroide de cada fibra 
					end do
				print *,' def',FibrasCon(1,2), FibrasCon(int(NF/2),2), FibrasCon(int(NF),2)	
				!Paso 10. Calcular el esfuerzo del concreto f_c en el centroide de cada fibra a compresión
					!recordar que el programa ya identifica que cuando Cn>=H todo el elemento está a compresión y se divide las fibras en H, cuando no se dividen en Cn
					do i=1 ,NF  
						if (FibrasCon(i,1)<(hc)) then !pregunta si concreto convencional
							if	(FibrasCon(i,2) .LE. (EPSIcu))  then !pregunta si no se ha fracturado el concreto 
								FibrasCon(i,3)= fckp(noCon,fpc,FibrasCon(i,2),zm)  !si no está fracturado esfuerzo no confinado en esa fibra
							else  !si está fracturado(en este criterio de falla ninguno puede estar fracturado)
								FibrasCon(i,3)=0   !no hay esfuerzo si está fracturado
								hfrac=(FibrasCon(i,1)) + DF/2  !la altura de esa fibra se convierte en la altura que ya se fracturó
								
							end if

						
						else   !si está debajo del primer castillo

							if (FibrasCon(i,1)>(Lm-hc)) then !pregunta si esta en el segundo castillo
								if	(FibrasCon(i,2) .LE. (EPSIcu))  then !pregunta si no se ha fracturado el concreto no conf.
									FibrasCon(i,3)= fckp(noCon,fpc,FibrasCon(i,2),zm)  !si no esta fracturado esfuerzo no confinado en esa fibra
								else  !si está fracturado. en este criterio de falla ninguno puede estar fracturado
									FibrasCon(i,3)=0   !no hay esfuerzo si está fracturado
									
									hfrac=(FibrasCon(i,1)) + DF/2  !la altura de esa fibra se convierte en la altura que ya se fracturó
								end if


							else !está en la zona de CCA la fibra está entre hc y lm-hc

								!para la parte del concreto no confinado
								
								if	(FibrasCon(i,2) .LE. (EPSIcuCCA))  then !pregunta si no se ha fracturado el CCA
									FibrasCon(i,3)= FibrasCon(i,2)*EcCCA  !si no esta fracturado esfuerzo CCA
								else  !si está fracturado
									FibrasCon(i,3)=0   !no hay esfuerzo si está fracturado
									
									hfrac=(FibrasCon(i,1)) + DF/2  !la altura de esa fibra se convierte en la altura que ya se fracturó, tomado como la parte inferior
								end if
									
							end if !fin del if que pregunta si está en el segundo castillo

						end if  !fin del if que pregunta si está debajo del primer castillo
                    
					end do !end del do del paso 10
				print *,' esf', FibrasCon(1,3), FibrasCon(int(NF/2),3), FibrasCon(int(NF),3)		
				!Paso 11. Calcular la fuerza de compresión de cada fibra multiplicando el esfuerzo f_c  por el área de cada fibra	
					do i=1 ,NF
						if (FibrasCon(i,1)<(hc)) then !pregunta si está en el primer castillo
							FibrasCon(i,4)=FibrasCon(i,3)*b*DF  !esfuerzo  *  base* altura de la fibra 
						else 
							if (FibrasCon(i,1)>(lm-hc)) then !pregunta si esta en el segundo castillo
								FibrasCon(i,4)=  FibrasCon(i,3)*b*DF !está en el segundo rectágulo no confinado
							else  !está en la zona de CCA
								FibrasCon (i,4)= FibrasCon(i,3)*b*DF  !esfuerzo no confi *  base* altura de la fibra								
							end if					
						end if 
					end do

				!Se saca la sumatoria de fuerzas de compresión
					
					do i=1, NF	
						Fcc=Fcc+FibrasCon(i,4)  !se suman todas la fuerzas de las fibras
					end do
						!print *,'  sumatoria comp',Fcc		
			end if   !fin del if que pregunta si entra o no el concreto a la sumatoria de fuerzas			
		print *,'1  Cn ', Cn, ' Phi ', PHIn, '    Cri falla ', mensaje
		!Paso 12. Calcular la profundidad de cada lecho del acero de refuerzo por flexión
			!este paso es un dato de entrada que se encuentra en la primera columna de la matriz As()
			do i=1, NL
				Asn(i,1)=As(i,2)   !profundidad del lecho
				Asn(i,4)=As(i,1)   !área de acero del lecho
			end do
		!Paso 13. Calcular la deformación unitaria del acero ∈_s para cada lecho
			do i=1, NL
				if ( Asn(i,1) < Cn ) then !se pregunta si está en la parte de compresión
					Asn(i,2)= (Abs(Cn-Asn(i,1)))*PHIn
				else !si está en la parte de tensión
					Asn(i,2)= (Asn(i,1)-Cn)* abs(PHIn)  
				end if
               
			end do
		!Paso 14. Calcular el esfuerzo f_s en cada lecho del acero (tensión o compresión).  
			do i=1, NL
				
				Asn(i,3)= fsm(TipoAcero,Asn(i,2),Es)  !se entra al modelo consti del tipo de acero con la def del lecho
				print*, 'esf ace',i, Asn(i,2), Asn(i,3) 
                    if ( Asn(i,3)==0 ) then  
						bandera=1                       !se reporta que la sección ya falló
						bandera2=1        !ya pasó a la zona de cambio de criterio
						Criterio = 'Fractura_del_acero_del_lecho_inferior ' 
                        print*,"se alcanza la fractura del acero"
						exit
						!print *,criterio , Cn
					
					end if					
			end do
        !se saca el esfuerzo de tensión en el concreto 
			Ftn=0
            if (Cn<Lm) then  !si pregunta si hay tensión en la sección
				EPSIc= Phin*(lm-Cn)
				Print*, EPSIc
				if (EPSIc>(EPSIfr*1.01)) then !pregutna si ya se fracturó el concreto celular 
					bandera=1                       !se reporta que la sección ya falló
					bandera2=1        !ya pasó a la zona de cambio de criterio
					Criterio = 'Fractura concreto'
					print*, "se fractura el concreto"
					
				else 
					if (Cn .GE. (Lm-hc)) then !pregunta si ya pasó el primer castillo
					 !está en el primer castillo
						Ft1= (EPSIc*Ec)*b*(lm-Cn)/2   !se saca directo lo esfuerzos!solo el primer castillo está a tensión, el esfeurzo en forma de triangulo
						ft2=0
						ft3=0
						ftn=ft1+ft2+ft3

					else  
						EPSIt1=(lm-hc-Cn)*PHIn
						Ft3=0 ! EPSIt1*EcCCA*b*(lm-hc-Cn)/2  !triángulo de esfuerzo de la zona de CCA !se desprecia
						ft2= (EPSIt1*Ec)*b*hc  !es el rectangulo del castillo
						Ft1= ((EPSIc*Ec)-(EPSIt1*Ec))*b*(hc)/2   !se saca directo lo esfuerzos, es el triangulo del castillo, con el triangulo hacen un trapecio
						ftn=ft1+ft2+ft3

					end if
				end if
						   
			else !Cn es mayor que lm por lo que solo hay compresión
				ftn=0
			end if

		

		!se revisa que no haya fallado el acero	
		if (bandera==1) then
            print*, criterio 
			print*, ' '
			cont2=cont2+1	
        end if 
        if (bandera==1) goto 10  

		!Paso 15. Calcular la fuerza en cada lecho del acero (tensión o compresión). 
			!fuerzas de compresión y tensión en el acero
				Fsc=0
				Fst=0
				do i=1, NL
					if ( Asn(i,1)<Cn ) then !se pregunta si está en la parte de compresión
						Fsc=Fsc+(Asn(i,3)*Asn(i,4))!se suman las fuerzas de compresión
					else 
						Fst=Fst+(Asn(i,3)*Asn(i,4))	!se suman las fuerzas de tensión
					end if
				end do
		!Paso 16. 

			!fuerzas de tensión en el concreto =0
			!se saca la sumatoria de fuerzas
				CT=Fcc+Fsc   !fuerzas de compresión del concreto conf o no conf, más fuerzas compresión del acero
				TT=Fst+ftn			!lo que aporta el acero
				P=CT-TT  !la carga axial es igual a la diferencia entre las fuerzas de compresión y tensión	!check
	
	print *, Fcc, Fsc, Fst, ftn,' CT ',CT , ' TT ', TT			
			 
		!MOMENTO 
			if (Cn>lm) then
			!Momento generados por el acero
				Msc=0
				Mst=0
				!se calcula los momentos en los lechos de acero
				do i=1, NL
					if ( Asn(i,1)<lm ) then !se pregunta si está en la parte de compresión
						Msc=Msc+ ((Asn(i,3)*Asn(i,4))*(Cn-Asn(i,1))) !el momento es igual a la fuerza en la fibra por el brazo de palanca
						!Msc=Msc+ Asn(i,3)*Asn(i,4)*(As(i,1))
					else !si está en la parte de tensión
						Mst=Mst+Asn(i,3)*Asn(i,4)*(Asn(i,1)-Cn)
						!Mst=Mst+Asn(i,3)*Asn(i,4)*(Asn(i,1))
					end if
				end do
			!Momento generados por el concreto
				Mcc=0
				do i=1,NF
					Mcc=Mcc+FibrasCon(i,4)*(Cn-FibrasCon(i,1))  !la fuerza en esa fibra por el brazo de palanca al centroide
					!Mcc=Mcc+FibrasCon(i,5)*(FibrasCon(i,1))  
				end do
			!se saca el momento para la Cn
				Mn=Mcc+Msc+Mst+(P*((lm/2)-Cn)) 
			else  !Cn < Lm
			!Momento generados por el acero
				Msc=0
				Mst=0
				!se calcula los momentos en los lechos de acero
				do i=1, NL
					if ( Asn(i,1)<Cn ) then !se pregunta si está en la parte de compresión
						Msc=Msc+ Asn(i,3)*Asn(i,4)*(Cn-Asn(i,1)) !el momento es igual a la fuerza en la fibra por el brazo de palanca
						!Msc=Msc+ Asn(i,3)*Asn(i,4)*(As(i,1))
					else !si está en la parte de tensión
						Mst=Mst+Asn(i,3)*Asn(i,4)*(Asn(i,1)-Cn)  !check
						!Mst=Mst+Asn(i,3)*Asn(i,4)*(Asn(i,1))
					end if
				end do
			!Momento generados por el concreto
				Mcc=0

				if (Cn .GT. 0) then
					do i=1,NF
						Mcc=Mcc+FibrasCon(i,4)*(Cn-FibrasCon(i,1))  !la fuerza en esa fibra por el brazo de palanca al centroide
						!Mcc=Mcc+FibrasCon(i,5)*(FibrasCon(i,1))  
					end do
				end if 

				if (Cn .GE. (Lm-hc)) then !pregunta si ya pasó el primer castillo
					 !está en el primer castillo
						mct1= Ft1*(2*(Lm-Cn)/3)!se saca directo lo esfuerzos!solo el primer castillo está a tensión, el esfeurzo en forma de triangulo
						mct2=0
						mct3=0

					else  
						mct1= ft1*((lm-hc-Cn)+2*hc/3)  !momento generado por el triangulo
						mct2=ft2*((lm-hc-Cn)+hc/2)   !momento generado por el rectángulo 
						mct3= ft3*2*(lm-hc-Cn)/3    !momento generado por el CCA
				end if
				
			!se saca el momento para la Cn
				Mn=Mcc+Msc+Mst+(P*((lm/2)-Cn))+mct1+mct2+mct3
			end if 

    print *, 'fin del ciclo Cn', ' M ', Mn, ' p ',P , bandera ,cont	
    print*, " "	

		if (Cn < 0)	then
			If ( abs(Mn)<100 ) then
				print*, "MN es muy pequeño"
				exit
			end if
		end if 
		if (hfrac2<hfrac) then
			hfrac2=hfrac
		end if 
        cont=cont+1	
		cont2=cont2+1
		!se asignan los valores a la matriz de 					
			MPC(cont,1)=Mn/hm
			MPC(cont,2)=P/(lm*b)
			MPC(cont,3)=Cn
			MPC(cont,4)=PHIn


        
	end do   !fin del do que cicla las Cn
			

		
			



!Impresión de resultados
 OPEN(UNIT=11, FILE="RESULTADOS.txt", STATUS="REPLACE", ACTION="WRITE")
	WRITE(11,*) '                       Nombre_de_la_prueba: ' , NOMBRE 
	WRITE(11,*) "    "
     Write (11,*) " Datos_de_entrada"
     write (11,*) ' b ', b,' lm ', Lm, 'hc', hc
     write (11,*) ' fpc ', fpc
     write(11,*)  "tipo_acero: ", TipoAcero, ' rho ', rho
	 write (11,*) "     Datos del acero " 
	 write (11,*) " diámetro      núm._de_barras         profundidad "
	do i=1, NL
	  write (11,*) AsE(i,1), AsE(i,2), AsE (i,3)
	end do
	 Write (11,*)  "altura_ini_fin_mod " , As(1,2), As(NL,2)
	Write (11,*) ' '
    Write (11,*) 'datos_estribos'

    Write (11,*) ' '
     Write (11,*) "Criterios_de_iteración"
     write (11,*) 'iter_máx_para_C', maxI, ' núm.de_fibras: ', NF
     write (11,*) " "
	 write (11,*) "                Resultados           "
	 write (11,*) " "
	 WRITE (11,*) "				Criterio_falla_concreto_NO_confinado"
	 write (11,*) "	Momento		Carga_axial		eje_Neutro		phi  "
     write (11,*)
     
     write (11,*)MPC (1,1), MPC (1,2),MPC (1,3), MPC (1,4)
	 do i=2, cont
	 	write (11,*)MPC (i,1), MPC (i,2), MPC (i,3), MPC (i,4)
	 end do
	 write (11,*)" "
	 write (11,*)" "



 CLOSE(11)   
contains 
	real function fpm(a,b)
		real::a,b, fpm
		fpm= (a+b)/2.0
	end function
		
END PROGRAM Program_LateralForce_axialLoad

!función para calcular fc del concreto según el modelo de Kent y Park modificado.    
 function fckp(k1,fpc1, EPSIc1,Zm1)
 IMPLICIT NONE
 REAL :: fckp 					 !Variable dummy
 REAL :: k1,fpc1, EPSIc1,Zm1      !!variables locales
	if (EPSIc1>0) then 
		if (EPSIc1<0.002*K1) then 
   	 		fckp=k1*fpc1*(((2*EPSIc1)/(0.002*k1))-(EPSIc1/(0.002*k1))**2)
		else 
		 	if (K1==1) then !se trata de concreto no confinado
				if (EPSIc1>0.004) then
					fckp=0 !el concreto no confinado está aplastado
				else !no está aplastado
					fckp=k1*fpc1*(1-(Zm1*(EPSIc1-0.002*k1)))
				end if
			else ! para el concreto confinado
				fckp=k1*fpc1*(1-(Zm1*(EPSIc1-0.002*k1)))
					if (fckp<0.2*k1*fpc1) then
						fckp=0.2*k1*fpc1
					end if
			end if
		end if 		
    else 
		fckp=0
	end if
 END function fckp

!función para calcular los esfuerzos en el acero dependiendo del tipo de acero y la deformación segun los modelos constitutivos
 function fsm (tipo,EPSIs1,Es1)  
 IMPLICIT NONe
 REAL :: fsm
 REAL :: fy1, EPSIs1, Es1,fu1, EPSIsh1, EPSIsu1, P1      !!variables locales
 CHARACTER(len=50):: Tipo
	if (EPSIs1>0) then 
	!acero grado 42 nominal con def. max. de de Rodríguez y Botero
		if (tipo=='G42') then
		 fy1=4200     !esfuerzo nominal a tensión
		 EPSIsu1=0.1295
			if (EPSIs1 .LE. (fy1/Es1)) then !pregunta si es menor a la def. de fluencia
   	 			fsm=Es1*EPSIs1    !está en el rango lineal
			else !sino está en el rango lineal
				if (EPSIs1 .LE. EPSIsu1) then  !pregunta si es menor  o igual a la def de falla
   	 			fsm=fy1     !está en la mesesta 
				else     !si es mayor a la de falla
					fsm=0  !ya falló
				end if
			end if 	
		end if		
	!acero grado 42 según el artículo de Rodriguez y Botero
		if (tipo=='G42RB') then
			!datos promedios
			fy1=4593 
			fu1=7463  
			EPSIsh1=0.0074
			EPSIsu1=0.1295
			P1=3.418
			if (EPSIs1>(fy1/Es1)) then        !pregunta si ya llegó a la fluencia
   	 			if (EPSIs1 >(EPSIsh1)) then    !pregunta si ya llegó a la def de endurecimiento
					if ( EPSIs1>EPSIsu1 ) then!pregunta si ya llegó a la falla
						fsm=0	!ya falló 
					else
						fsm=fu1+(fy1-fu1)*((EPSIsu1-EPSIs1)/(EPSIsu1-EPSIsh1))**P1  !está en la curva de endurecimiento
					end if
				else
					fsm=fy1  !está en la meseta plástica
				end if
			else 
				fsm=Es1*EPSIs1	!está en el rango elástico lineal
			end if 		
		end if!fin del G42 Rod. y bot. 
	
	!acero grado 42 tarea, según está en el archivo de las inst. de la tarea
		if (tipo=='G42T') then
			!datos obtenidos
			fy1=5219
			EPSIsh1=0.0172
			EPSIsu1=0.175
			if (EPSIs1>(fy1/Es1)) then        !pregunta si ya llegó a la fluencia
   	 			if (EPSIs1 >(EPSIsh1)) then    !pregunta si ya llegó a la def de endurecimiento
					if ( EPSIs1>EPSIsu1 ) then!pregunta si ya llegó a la falla
						fsm=0	!ya falló 
					else
						fsm=745488*EPSIs1**3 -316980*EPSIs1**2 + 45700*EPSIs1+4613.7!está en la curva de endurecimiento
					end if
				else
					fsm=fy1  !está en la meseta plástica
				end if
			else 
				fsm=Es1*EPSIs1	!está en el rango elástico lineal
			end if 		
		end if


	!acero grado 56 nominal 
		if (tipo=='G56') then
		fy1=5600
		EPSIsu1=0.15
			if (EPSIs1<(fy1/Es1)) then !pregunta si es menor a la def. de fluencia
   	 			fsm=Es1*EPSIs1    !está en el rango lineal
			else !sino está en el rango lineal
				if (EPSIs1 .LE. EPSIsu1) then  !pregunta si es menor o igual a la def de falla
   	 			fsm=fy1     !está en la mesesta 
				else     !si es mayor a la de falla
					fsm=0  !ya falló
				end if
			end if 	
		end if		

	!acero grado 56 tarea, según está en el archivo de las inst. de la tarea
		if (tipo=='G56T') then
			!datos obtenidos
			fy1=5887.9 
			EPSIsh1=0.0148
			EPSIsu1=0.15
			if (EPSIs1>(fy1/Es1)) then        !pregunta si ya llegó a la fluencia
   	 			if (EPSIs1 >(EPSIsh1)) then    !pregunta si ya llegó a la def de endurecimiento
					if ( EPSIs1>EPSIsu1 ) then!pregunta si ya llegó a la falla
						fsm=0	!ya falló 
					else
						fsm=1000000*EPSIs1**3 - 443329*EPSIs1**2 + 53694*EPSIs1+5337.8!está en la curva de endurecimiento
					end if
				else
					fsm=fy1  !está en la meseta plástica
				end if
			else 
				fsm=Es1*EPSIs1	!está en el rango elástico lineal
			end if 		
		end if

	!acero grado 60-modelo bilineal con endurecimiento por deformación lineal con datos de Ramírez (2021)
		if (tipo=='G60') then
			fy1=8086
			EPSIsu1=0.12
			if (EPSIs1 .LE. (fy1/Es1)) then !pregunta si es menor a la def. de fluencia
   	 			fsm=Es1*EPSIs1    !está en el rango lineal
			else !sino está en el rango lineal
				if (EPSIs1 .LE. EPSIsu1) then  !pregunta si es menor a la def de falla
   	 			fsm=(((8530-fy1)/(0.12-(fy1/Es1)))*EPSIs1)+fy1     !está en la mesesta con ligera pendiente
				else     !si es mayor a la de falla
					fsm=0  !ya falló
				end if
			end if 		
		end if

	
    else 
		fsm=0        !se introdujo una deformación negativa
	end if

 	END function fsm

!FUNCIONES PARA CALCULAR DEFORMACIONES 

 !función para calcular EPSIc del concreto según el modelo de Kent y Park modificado.    
  function EPSIkp(k1,fpc1, fc1,Zm1)
  IMPLICIT NONE
  REAL :: EPSIkp 					 !Variable dummy
  REAL :: k1,fc1, fpc1,Zm1      !!variables locales
	if (fc1>0) then 
		if (fc1<fpc1*K1) then 
   	 		EPSIkp=0.002*k1*(fpc1+SQRT((fpc1**2)-((fc1*fpc1)/k1)))
		else 
			EPSIkp=((1-fc1/(k1*fpc1))/Zm1)+0.002*k1  
				
		end if 		
    else 
		EPSIkp=0
	end if
  END function EPSIkp


